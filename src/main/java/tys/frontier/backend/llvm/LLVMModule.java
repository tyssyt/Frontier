package tys.frontier.backend.llvm;

import com.google.common.base.Joiner;
import com.google.common.primitives.Ints;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.javacpp.PointerPointer;
import org.bytedeco.llvm.LLVM.*;
import tys.frontier.State;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.literal.FStringLiteral;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.logging.Log;
import tys.frontier.util.Utils;

import java.io.IOException;
import java.nio.file.Path;
import java.util.*;

import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toList;
import static org.bytedeco.llvm.global.LLVM.*;
import static tys.frontier.backend.llvm.LLVMUtil.*;

public class LLVMModule implements AutoCloseable {

    private enum Linkage{
        PRIVATE(LLVMPrivateLinkage),
        EXTERNAL(LLVMExternalLinkage);

        final int type;

        Linkage(int type) {
            this.type = type;
        }

        static Linkage findLinkage(boolean isNative) {
            if (isNative)
                return Linkage.EXTERNAL;
            else
                return Linkage.PRIVATE;
        }
    }

    private static final int CALLING_CONVENTION = LLVMFastCallConv;

    private static final int TRUE = 1;
    private static final int FALSE = 0;

    final LLVMTypeRef byteType;
    final LLVMTypeRef bytePointer;
    final LLVMTypeRef bytePointerPointer;

    private boolean verificationNeeded = false;
    private boolean ownsContext;
    private LLVMContextRef context;
    private LLVMModuleRef module;
    private Map<FType, LLVMTypeRef> llvmTypes = new HashMap<>();
    private Map<FType, LLVMValueRef> typeInfo = new HashMap<>();
    private Map<String, LLVMValueRef> constantStrings = new HashMap<>();
    private Object2IntMap<FField> fieldIndices = new Object2IntOpenHashMap<>();
    private List<FClass> todoClassBodies = new ArrayList<>();
    private List<FField> todoFieldInitilizers = new ArrayList<>();
    private List<FFunction> todoFunctionBodies = new ArrayList<>();

    public LLVMModule(String name) {
        this(name, LLVMGetGlobalContext(), false);
    }

    public LLVMModule(String name, LLVMContextRef context, boolean ownsContext) {
        this.context = context;
        this.module = LLVMModuleCreateWithNameInContext(name, context);
        this.ownsContext = ownsContext;
        byteType = LLVMInt8TypeInContext(context);
        bytePointer = LLVMPointerType(byteType, 0);
        bytePointerPointer = LLVMPointerType(bytePointer, 0);
        fillInPredefinedTypes();
    }

    private void fillInPredefinedTypes() {
        llvmTypes.put(FBool.INSTANCE, LLVMInt1TypeInContext(context));
        llvmTypes.put(FOptional.from(FBool.INSTANCE), LLVMIntTypeInContext(context, 2));
        llvmTypes.put(FFloat32.INSTANCE, LLVMFloatTypeInContext(context));
        llvmTypes.put(FFloat64.INSTANCE, LLVMDoubleTypeInContext(context));
        llvmTypes.put(FTuple.VOID, LLVMVoidTypeInContext(context));
        llvmTypes.put(FNull.NULL_TYPE, bytePointer);
    }

    @Override
    public void close() {
        LLVMDisposeModule(module);
        if (ownsContext)
            LLVMContextDispose(context);
    }

    LLVMModuleRef getModule() {
        return module;
    }

    LLVMContextRef getContext() {
        return context;
    }

    LLVMBuilderRef createBuilder() {
        return LLVMCreateBuilderInContext(this.context);
    }

    LLVMTypeRef getLlvmType (FType fClass) { //TODO needs sync for multithreading
        LLVMTypeRef res = llvmTypes.get(fClass);
        if (res != null) {
            return res;
        } else if (fClass instanceof FIntN) {
            res = LLVMIntTypeInContext(context, ((FIntN) fClass).getN());
        } else if (fClass instanceof FArray) {
            res = arrayType(((FArray) fClass), 0);
        } else if (fClass instanceof CArray) {
            res = cArrayType(((CArray) fClass));
        } else if (fClass instanceof FOptional) {
            res = getLlvmType(((FOptional) fClass).getBaseType());
        } else if (fClass instanceof FFunctionType) {
            res = functionType(((FFunctionType) fClass));
        } else if (fClass instanceof FTuple) {
            res = tupleType((FTuple) fClass);
        } else {
            Utils.NYI("LLVM type for: " + fClass);
        }
        llvmTypes.put(fClass, res);
        return res;
    }

    LLVMValueRef getTypeInfo(FType fClass) {
        LLVMValueRef res = typeInfo.get(fClass);
        if (res != null)
            return res;
        LLVMValueRef name = constantString(fClass.getIdentifier().name);
        LLVMValueRef castedName = LLVMConstBitCast(name, getLlvmType(FStringLiteral.TYPE));
        LLVMTypeRef typeInfoType = LLVMGetElementType(getLlvmType(FTypeType.INSTANCE));
        LLVMValueRef typeInfo = LLVMConstNamedStruct(typeInfoType, castedName, 1);

        res = LLVMAddGlobal(this.module, typeInfoType, getTypeInfoName(fClass));
        setGlobalAttribs(res, Linkage.PRIVATE, true);
        LLVMSetGlobalConstant(res, TRUE);
        LLVMSetInitializer(res, typeInfo);
        this.typeInfo.put(fClass, res);
        return res;
    }

    private LLVMTypeRef arrayType(FArray type, int length) {
        PointerPointer<LLVMTypeRef> types = new PointerPointer<>(getLlvmType(FIntN._32),
                LLVMArrayType(getLlvmType(type.getBaseType()), length));
        LLVMTypeRef baseType = LLVMStructTypeInContext(context, types, 2, FALSE);
        return LLVMPointerType(baseType, 0);
    }

    private LLVMTypeRef cArrayType(CArray type) {
        return LLVMPointerType(getLlvmType(type.getBaseType()), 0);
    }

    private LLVMTypeRef functionType(FFunctionType functionType) {
        LLVMTypeRef baseType;
        if (functionType.getIn() instanceof FTuple) {
            List<FType> types = ((FTuple) functionType.getIn()).getTypes();
            PointerPointer<LLVMTypeRef> params = LLVMUtil.createPointerPointer(types, this::getLlvmType);
            baseType = LLVMFunctionType(getLlvmType(functionType.getOut()), params, types.size(), FALSE);
        } else {
            baseType = LLVMFunctionType(getLlvmType(functionType.getOut()), getLlvmType(functionType.getIn()), 1, FALSE);
        }
        return LLVMPointerType(baseType, 0);
    }

    /**
     * @param function function for which we want a LLVM-Function-Type
     * @return the LLVM-Function-Type corresponding to the FFunction
     */
    private LLVMTypeRef stupidHackToGetFunctionTypeWithCorrectTupleUnpackingWhyyyyyyyy(FFunction function) { //TODO you guessed it...
        List<FParameter> fParams = function.getSignature().getParameters();
        PointerPointer<LLVMTypeRef> params = createPointerPointer(fParams, p -> getLlvmType(p.getType()));
        LLVMTypeRef returnType = getLlvmType(function.getType());
        return LLVMFunctionType(returnType, params, fParams.size(), FALSE);
    }


    private LLVMTypeRef tupleType(FTuple tuple) {
        assert tuple != FTuple.VOID;
        PointerPointer<LLVMTypeRef> types = createPointerPointer(tuple.getTypes(), this::getLlvmType);
        return LLVMStructTypeInContext(context, types, tuple.arity(), FALSE);
    }

    LLVMValueRef constantString(String s) { //TODO needs sync for multithreading
        //TODO make sure strings/string wrappers can be unified across modules etc..
        LLVMValueRef res = constantStrings.get(s);
        if (res == null) {
            res = LLVMAddGlobal(this.module, LLVMGetElementType(arrayType(FStringLiteral.TYPE, s.length())), getConstantStringName(s));
            setGlobalAttribs(res, Linkage.PRIVATE, true);
            LLVMSetGlobalConstant(res, TRUE);

            LLVMValueRef size = LLVMConstInt(getLlvmType(FIntN._32), s.length(), FALSE);
            LLVMValueRef rawString = LLVMConstStringInContext(this.context, s, s.length(), TRUE);
            LLVMValueRef string = LLVMConstStructInContext(context, new PointerPointer<>(size, rawString), 2, FALSE);
            LLVMSetInitializer(res, string);
            constantStrings.put(s, res);
        }
        return res;
    }

    int getFieldIndex(FField field) {
        return fieldIndices.getInt(field);
    }

    /**
     * Parses all class types found in the file, creating corresponding LLVM types in this module.
     * @param namespaces namespaces to parse
     */
    public void parseTypes(Iterable<DefaultNamespace> namespaces) {
        verificationNeeded = true;
        for (DefaultNamespace namespace : namespaces) {
            FClass fClass = namespace.getType();
            if (fClass == null)
                continue;
            if (fClass.isPredefined())
                continue;
            parseClass(fClass);
            todoClassBodies.add(fClass);
        }
    }

    private void parseClass(FType clazz) {
        LLVMTypeRef baseType = LLVMStructCreateNamed(context, getClassName(clazz));
        LLVMTypeRef pointerType = LLVMPointerType(baseType, 0);
        LLVMTypeRef old = llvmTypes.put(clazz, pointerType);
        if (old != null)
            Utils.cantHappen();
    }

    /**
     * Parses all function Headers found in the file, creating corresponding function prototypes in this module.
     * Should be called after {@link #parseTypes}.
     * @param namespaces namespaces to parse
     */
    public void parseClassMembers(Iterable<DefaultNamespace> namespaces) {
        verificationNeeded = true;
        //TODO initializers for fields that are done in the fields
        for (DefaultNamespace namespace : namespaces) {
            for (Signature signature : namespace.getFunctions(false).values()) {
                FFunction function = signature.getFunction();
                if (!function.isPredefined()) {
                    addFunctionHeader(function);
                    if (!function.isNative())
                        todoFunctionBodies.add(function);
                }
            }

            FClass fClass = namespace.getType();
            if (fClass == null)
                continue;
            for (FField field : fClass.getStaticFields().values()) {
                //TODO see if the initializer is a const and direclty init here instead of the block?
                //TODO see if something can be done for final?
                //TODO optimizer flags like we don't care bout the address and readonly
                //TODO for final and effective final fields of objects the pointer pointer could be lowered into a pointer...
                LLVMTypeRef type = getLlvmType(field.getType());
                LLVMValueRef global = LLVMAddGlobal(module, type, getStaticFieldName(field));

                setGlobalAttribs(global, Linkage.findLinkage(false), false);
                todoFieldInitilizers.add(field);
            }
        }
    }

    /**
     * Parses function header and adds a Prototype to this module.
     * Does not generate code for the body.
     * @param function function to add
     * @return Reference to the LLVM function
     */
    private LLVMValueRef addFunctionHeader(FFunction function) { //TODO find other good attributes to set for function and parameters
        LLVMValueRef res = LLVMAddFunction(module, getFunctionName(function), stupidHackToGetFunctionTypeWithCorrectTupleUnpackingWhyyyyyyyy(function));
        //set global attribs
        setGlobalAttribs(res, Linkage.findLinkage(function.isNative()), true);
        //LLVMSetFunctionCallConv(res, CALLING_CONVENTION); TODO this crashes the program, but it should work... , maybe its because of the c links ?

        //set names for all arguments, add parameter attributes
        //TODO we can use dereferenceable<i> instead of nonNull, and use dereferenceable_or_null for all others, but we need the type sizes...
        LLVMAttributeRef nonNullAttr = getEnumAttribute("nonnull");
        List<FParameter> fParams = function.getSignature().getParameters();
        for (int i=0; i<fParams.size(); i++) {
            FParameter param = fParams.get(i);
            LLVMSetValueName(LLVMGetParam(res, i), param.getIdentifier().name);
            if (LLVMGetTypeKind(getLlvmType(param.getType())) == LLVMPointerTypeKind && !(param.getType() instanceof FOptional))
                LLVMAddAttributeAtIndex(res, 1+i, nonNullAttr);
        }

        //set return type attributes
        if (function.getType() != FTuple.VOID
                && LLVMGetTypeKind(getLlvmType(function.getType())) == LLVMPointerTypeKind
                && !(function.getType() instanceof FOptional))
            LLVMAddAttributeAtIndex(res, 0, nonNullAttr);
        return res;
    }

    /**
     * Creates LLVM Code for all parsed Functions and Classes in this module.
     * Should be called after {@link #parseClassMembers}.
     */
    public void fillInBodies(Collection<DefaultNamespace> namespaces, FFunction entryPoint) {//TODO consider parallelizing this, but first check how much LLVM likes in module parallelization
        verificationNeeded = true;

        //start with filling in the bodies for missing types
        for (FClass fClass : todoClassBodies) {
            List<LLVMTypeRef> subtypes = new ArrayList<>();
            int index = 0;
            for (FField field : fClass.getInstanceFields().values()) {
                subtypes.add(getLlvmType(field.getType()));
                fieldIndices.put(field, index++);
            }
            LLVMStructSetBody(LLVMGetElementType(getLlvmType(fClass)), createPointerPointer(subtypes), subtypes.size(), FALSE);
        }

        try (LLVMTransformer trans = new LLVMTransformer(this)) {
            for (FField field : todoFieldInitilizers) {
                trans.visitField(field);
            }
            //bodies for fields
            for (FFunction function : todoFunctionBodies)
                trans.visitFunction(function);

            //main
            if (entryPoint != null)
                generateMain(trans, namespaces, entryPoint);
        }

    }

    private void generateMain(LLVMTransformer trans, Collection<DefaultNamespace> namespaces, FFunction entryPoint) {
        //TODO if isWindows
        for (DefaultNamespace namespace : namespaces) {
            if (namespace.getIdentifier().name.equals("WinMainArgs")) { //TODO find a less stupid solution
                FClass fClass = namespace.getType();
                FField hInstance = fClass.getStaticFields().get(new FIdentifier("hInstance"));
                FField nCmdShow = fClass.getStaticFields().get(new FIdentifier("nCmdShow"));
                if (hInstance != null || nCmdShow != null) {
                    trans.generateWinMain(entryPoint, hInstance, nCmdShow);
                    return;
                }
            }
        }
        trans.generateMain(entryPoint);
    }

    public void createMetaData() {
        if (!llvmTypes.containsKey(FTypeType.INSTANCE) || !FTypeType.INSTANCE.getStaticFields().containsKey(FTypeType.allTypes_ID))
            return;

        //TODO once code can handle both pointers and direct types, we no longer need to create in "intermediate diret type"
        //create intermediate sf
        List<LLVMValueRef> elements = llvmTypes.keySet().stream().sorted(comparing(FType::getIdentifier)).map(this::getTypeInfo).collect(toList());
        LLVMValueRef arr = LLVMConstArray(getLlvmType(FTypeType.INSTANCE), LLVMUtil.createPointerPointer(elements), elements.size());
        PointerPointer<LLVMValueRef> member = createPointerPointer(LLVMConstInt(getLlvmType(FIntN._32), elements.size(), FALSE), arr);
        LLVMValueRef struct = LLVMConstStructInContext(context, member, 2, FALSE);

        LLVMValueRef intermediate = LLVMAddGlobal(this.module, LLVMTypeOf(struct), "sf.allTypes.i");
        setGlobalAttribs(intermediate, Linkage.PRIVATE, true);
        LLVMSetGlobalConstant(intermediate, TRUE);
        LLVMSetInitializer(intermediate, struct);

        //set allTypes
        LLVMValueRef allTypes = LLVMGetNamedGlobal(module, getStaticFieldName(FTypeType.allTypes));
        LLVMSetInitializer(allTypes, LLVMConstBitCast(intermediate, getLlvmType(FTypeType.allTypes.getType())));
    }


    public void verify() { //TODO this should be called at other places as well
        if (!verificationNeeded)
            return;
        BytePointer outMassage = new BytePointer();
        if (LLVMVerifyModule(module, 1, outMassage) == 1) {
            String s = outMassage.getString();
            LLVMDisposeMessage(outMassage);
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                Utils.handleException(e);
            }
            Utils.handleError(s);
        }
        verificationNeeded = false;
    }

    private LLVMPassManagerRef createPassManager(int optimizationLevel) {
        optimizationLevel = Ints.constrainToRange(optimizationLevel, 0, 3);
        LLVMPassManagerRef res = LLVMCreatePassManager();
        LLVMPassManagerBuilderRef builder = LLVMPassManagerBuilderCreate();
        LLVMPassManagerBuilderSetOptLevel(builder, optimizationLevel);
        LLVMPassManagerBuilderPopulateFunctionPassManager(builder, res);
        LLVMPassManagerBuilderPopulateModulePassManager(builder, res);
        LLVMPassManagerBuilderDispose(builder);
        //TODO check if that really is all the passes we want or if we maybe want to set them manually, cmp. passes from bytedeco ex. and kaleidoscope
        return res;
    }

    public void optimize(int optimizationLevel) {
        LLVMPassManagerRef passManager = createPassManager(optimizationLevel);
        LLVMRunPassManager(passManager, module);
        LLVMDisposePassManager(passManager);
    }

    //Debug commands

    public void dump() { //TODO the emitToString kinda makes this irrelevant
        LLVMDumpModule(module);
    }

    public void viewFunctionControlFlowGraph(FFunction function) {
        LLVMValueRef res = LLVMGetNamedFunction(module, function.getIdentifier().name);
        if (res.isNull())
            return;
        LLVMViewFunctionCFG(res);
        //LLVMViewFunctionCFGOnly(res);
    }

    public String emitToString() {
        BytePointer out = LLVMPrintModuleToString(module);
        String res = out.getString();
        LLVMDisposeMessage(out);
        return res;
    }

    public void emitToFile(LLVMBackend.OutputFileType fileType, String fileName, List<Path> userLibs) { //TODO basically the first two are simple, the latter will need more options like target machine etc.
        BytePointer error = new BytePointer();
        int errorId = 0;
        switch (fileType) {
            case LLVM_IR:
                errorId = LLVMPrintModuleToFile(module, fileName, error);
                break;
            case LLVM_BITCODE:
                errorId = LLVMWriteBitcodeToFile(module, fileName);
                break;
            case TEXTUAL_ASSEMBLY:
                try (Target target = Target.getDefault()) {
                    errorId = target.emitToFile(module, fileName, LLVMAssemblyFile, error);
                }
                break;
            case NATIVE_OBJECT:
                try (Target target = Target.getDefault()) {
                    errorId = target.emitToFile(module, fileName, LLVMObjectFile, error);
                }
                break;
            case EXECUTABLE:
                String tempName = State.get().getTempDir().getPath() + fileName.substring(fileName.lastIndexOf(Utils.filesep)) + "_temp.o";
                Log.info(this, "writing temporary object file to: " + tempName);
                try (Target target = Target.getDefault()) {
                    errorId = target.emitToFile(module, tempName, LLVMObjectFile, error);
                    if (errorId != 0)
                        break;
                    ProcessBuilder linkerCall = Linker.buildCall(tempName, fileName, userLibs, target.getTargetTriple());
                    Log.info(this, "calling Linker: " + Joiner.on(' ').join(linkerCall.command()));
                    Process p = linkerCall.inheritIO().start();
                    p.waitFor();
                } catch (IOException | InterruptedException e) {
                    Utils.handleException(e);
                }
                break;
        }
        if (errorId != 0) {
            String message = error.getString();
            LLVMDisposeMessage(error);
            Utils.handleError(message);
        }
    }

    private void setGlobalAttribs(LLVMValueRef global, Linkage linkage, boolean unnamedAddr) {
        LLVMSetLinkage(global, linkage.type);
        LLVMSetUnnamedAddr(global, unnamedAddr ? TRUE : FALSE);
    }

    private LLVMAttributeRef getEnumAttribute(String name) {
        return getEnumAttribute(name, 0);
    }

    private LLVMAttributeRef getEnumAttribute(String name, int value) {
        int id = LLVMGetEnumAttributeKindForName(name, name.length());
        return LLVMCreateEnumAttribute(context, id, value);
    }

    private LLVMAttributeRef getStringAttribute(String name, String value) {
        return LLVMCreateStringAttribute(context, name, name.length(), value, value.length());
    }

}

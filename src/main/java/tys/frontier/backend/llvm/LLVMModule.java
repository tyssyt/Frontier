package tys.frontier.backend.llvm;

import com.google.common.primitives.Ints;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.javacpp.PointerPointer;
import org.bytedeco.llvm.LLVM.*;
import tys.frontier.State;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.InstanceField;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.NativeDecl;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.literal.FStringLiteral;
import tys.frontier.code.module.Include;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.logging.Log;
import tys.frontier.util.FileUtils;
import tys.frontier.util.Joiners;
import tys.frontier.util.OS;
import tys.frontier.util.Utils;

import java.io.IOException;
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

        static Linkage findLinkage(NativeDecl nativeDecl) {
            if (nativeDecl != null)
                return Linkage.EXTERNAL;
            else
                return Linkage.PRIVATE;
        }
    }

    private static final int CALLING_CONVENTION = LLVMFastCallConv;

    private static final int TRUE = 1;
    private static final int FALSE = 0;

    private LLVMValueRef constEmptyArray;

    final LLVMTypeRef byteType;
    final LLVMTypeRef bytePointer;
    final LLVMTypeRef bytePointerPointer;

    private boolean verificationNeeded = false;
    private boolean ownsContext;
    private LLVMContextRef context;
    private LLVMModuleRef module;
    private boolean debug;
    private Map<FType, LLVMTypeRef> llvmTypes = new HashMap<>();
    private Map<FClass, LLVMValueRef> typeInfo = new HashMap<>();
    private Map<String, LLVMValueRef> constantStrings = new HashMap<>();
    private Object2IntMap<FField> fieldIndices = new Object2IntOpenHashMap<>();
    private List<FClass> todoClassBodies = new ArrayList<>();
    private List<FField> todoFieldInitilizers = new ArrayList<>();
    private List<FFunction> todoFunctionBodies = new ArrayList<>();

    public LLVMModule(String name, boolean debug) {
        this(name, LLVMGetGlobalContext(), false, debug);
    }

    public LLVMModule(String name, LLVMContextRef context, boolean ownsContext, boolean debug) {
        this.context = context;
        this.module = LLVMModuleCreateWithNameInContext(name, context);
        this.ownsContext = ownsContext;
        this.debug = debug;
        byteType = LLVMInt8TypeInContext(context);
        bytePointer = LLVMPointerType(byteType, 0);
        bytePointerPointer = LLVMPointerType(bytePointer, 0);
        fillInPredefinedTypes();
        constEmptyArray = initConstEmptyArray();
    }

    private void fillInPredefinedTypes() {
        llvmTypes.put(FBool.INSTANCE, LLVMInt1TypeInContext(context));
        llvmTypes.put(FOptional.from(FBool.INSTANCE), LLVMIntTypeInContext(context, 2));
        llvmTypes.put(FFloat32.INSTANCE, LLVMFloatTypeInContext(context));
        llvmTypes.put(FFloat64.INSTANCE, LLVMDoubleTypeInContext(context));
        llvmTypes.put(FTuple.VOID, LLVMVoidTypeInContext(context));
        llvmTypes.put(FNull.NULL_TYPE, bytePointer);
    }

    private LLVMValueRef initConstEmptyArray() {
        //TODO this seems to complex, isn't it just literally a pointer to a int32 0? Or are there architectures where an empty array consumes memory?
        LLVMTypeRef intType = getLlvmType(FIntN._32);
        LLVMTypeRef arrayType = LLVMArrayType(bytePointer, 0);
        LLVMTypeRef type = LLVMStructTypeInContext(context, createPointerPointer(intType, arrayType), 2, FALSE);

        LLVMValueRef res = LLVMAddGlobal(module, type, "emptyArray");
        setGlobalAttribs(res, Linkage.PRIVATE, true);
        LLVMSetGlobalConstant(res, TRUE);
        LLVMSetInitializer(res, LLVMConstNull(type)); //empty array is {0, []}, which is a zero-initializer
        return res;
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
        return LLVMCreateBuilderInContext(context);
    }

    LLVMDIBuilderRef createDebugInfoBuilder() {
        return LLVMCreateDIBuilderDisallowUnresolved(module);
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

    LLVMValueRef getTypeInfo(FType type) {
        FClass fClass = (FClass) type;
        LLVMValueRef res = typeInfo.get(fClass);
        if (res != null)
            return res;

        LLVMTypeRef typeInfoType = LLVMGetElementType(getLlvmType(FTypeType.INSTANCE));
        res = LLVMAddGlobal(this.module, typeInfoType, getTypeInfoName(fClass));
        setGlobalAttribs(res, Linkage.PRIVATE, true);
        LLVMSetGlobalConstant(res, TRUE);
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
            res = LLVMAddGlobal(this.module, LLVMGetElementType(arrayType(FStringLiteral.TYPE, s.length()+1)), getConstantStringName(s));
            setGlobalAttribs(res, Linkage.PRIVATE, true);
            LLVMSetGlobalConstant(res, TRUE);

            LLVMValueRef size = LLVMConstInt(getLlvmType(FIntN._32), s.length(), FALSE);
            LLVMValueRef rawString = LLVMConstStringInContext(this.context, s + '\0', s.length()+1, TRUE);
            LLVMValueRef string = LLVMConstStructInContext(context, new PointerPointer<>(size, rawString), 2, FALSE);
            LLVMSetInitializer(res, string);
            constantStrings.put(s, res);
        }
        return res;
    }

    int getFieldIndex(FField field) {
        return fieldIndices.getInt(field);
    }

    LLVMValueRef getIntIntrinsicFunction(String name, FIntN type) {
        String fullName = name + ".i" + type.getN();
        LLVMValueRef res = LLVMGetNamedFunction(module, fullName);
        if (res == null) {
            LLVMTypeRef intType = getLlvmType(type);
            LLVMTypeRef functionType;
            switch (name) {
                case "llvm.ctlz", "llvm.cttz" ->
                    functionType = LLVMFunctionType(intType, createPointerPointer(intType, LLVMInt1TypeInContext(context)), 2, FALSE);
                case "llvm.smul.with.overflow", "llvm.umul.with.overflow" -> {
                    LLVMTypeRef returnType = getLlvmType(FTuple.from(type, FBool.INSTANCE));
                    functionType = LLVMFunctionType(returnType, createPointerPointer(intType, intType), 2, FALSE);
                }
                default -> functionType = LLVMFunctionType(intType, intType, 1, FALSE);
            }
            res = LLVMAddFunction(module, fullName, functionType);
        }
        return res;
    }

    LLVMValueRef getFloatIntrinsicFunction(String name, FFloat type) {
        String fullName = name + ".f" + type.getBits();
        LLVMValueRef res = LLVMGetNamedFunction(module, fullName);
        if (res == null) {
            LLVMTypeRef floatType = getLlvmType(type);
            res = LLVMAddFunction(module, fullName, LLVMFunctionType(floatType, floatType, 1, FALSE));
        }
        return res;
    }

    LLVMValueRef getMemcopyInstrinsic() {
        String fullName = "llvm.memcpy.p0i8.p0i8.i32";
        LLVMValueRef res = LLVMGetNamedFunction(module, fullName);
        if (res == null) {
            LLVMTypeRef functionType = LLVMFunctionType(LLVMVoidType(), createPointerPointer(bytePointer, bytePointer, getLlvmType(FIntN._32), LLVMInt1TypeInContext(context)), 4, FALSE);
            res = LLVMAddFunction(module, fullName, functionType);
        }
        return res;
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
        FClass fClass = (FClass) clazz;
        LLVMTypeRef baseType = LLVMStructCreateNamed(context, getClassName(fClass));
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
                    if (function.getNative() == null)
                        todoFunctionBodies.add(function);
                }
            }

            for (FField field : namespace.getStaticFields().values()) {
                //TODO see if the initializer is a const and direclty init here instead of the block?
                //TODO see if something can be done for final?
                //TODO optimizer flags like we don't care bout the address and readonly
                //TODO for final and effective final fields of objects the pointer pointer could be lowered into a pointer...
                LLVMTypeRef type = getLlvmType(field.getType());
                LLVMValueRef global = LLVMAddGlobal(module, type, getStaticFieldName(field));

                setGlobalAttribs(global, Linkage.findLinkage(null), false);
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
        setGlobalAttribs(res, Linkage.findLinkage(function.getNative()), true);
        //if (!function.isNative()) {
        //    LLVMSetFunctionCallConv(res, CALLING_CONVENTION); //TODO this crashes the program, but it should work... , maybe its because of the c links ?
        //}

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
            for (InstanceField field : fClass.getInstanceFields().values()) {
                LLVMTypeRef llvmType = getLlvmType(field.getType());
                //TODO if we have it, this can become field.isEmbedded && !field.getType().isDefaultEmbedded
                if (field.isEmbedded() && LLVMGetTypeKind(llvmType) == LLVMPointerTypeKind) {
                    llvmType = LLVMGetElementType(llvmType);
                }
                subtypes.add(llvmType);
                fieldIndices.put(field, index++);
            }
            LLVMStructSetBody(LLVMGetElementType(getLlvmType(fClass)), createPointerPointer(subtypes), subtypes.size(), FALSE);
        }

        try (LLVMTransformer trans = new LLVMTransformer(this, entryPoint, debug)) {
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
        if (OS.isWindows()) {
            for (DefaultNamespace namespace : namespaces) {
                if (namespace.getIdentifier().name.equals("WinMainArgs")) { //TODO find a less stupid solution
                    FClass fClass = namespace.getType();
                    FField hInstance = fClass.getNamespace().getStaticFields().get(new FIdentifier("hInstance"));
                    FField nCmdShow = fClass.getNamespace().getStaticFields().get(new FIdentifier("nCmdShow"));
                    if (hInstance != null || nCmdShow != null) {
                        trans.generateWinMain(entryPoint, hInstance, nCmdShow);
                        return;
                    }
                }
            }
        }
        trans.generateMain(entryPoint);
    }

    public void createMetaData() {
        if (llvmTypes.containsKey(FTypeType.INSTANCE) && FTypeType.INSTANCE.getNamespace().getStaticFields().containsKey(FTypeType.allTypes_ID)) {
            //TODO once code can handle both pointers and direct types, we no longer need to create in "intermediate direct type"
            //create intermediate sf
            List<LLVMValueRef> elements = llvmTypes.keySet().stream()
                    .sorted(comparing(FType::getIdentifier)).map(this::getTypeInfo).collect(toList());
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

        //fill type Info
        Queue<FClass> todo = new ArrayDeque<>(typeInfo.keySet());
        while (!todo.isEmpty()) {
            fillTypeInfo(todo.remove(), todo);
        }

    }

    private void fillTypeInfo(FType type, Queue<FClass> todo) {
        FClass fClass = (FClass) type;
        LLVMValueRef res = typeInfo.get(fClass);

        LLVMValueRef name = constantString(fClass.getIdentifier().name);
        LLVMValueRef castedName = LLVMConstBitCast(name, getLlvmType(FStringLiteral.TYPE));

        LLVMValueRef fields = constFieldsForTypeInfo(fClass, todo);
        LLVMValueRef castedFields = LLVMConstBitCast(fields, getLlvmType(FArray.getArrayFrom(FFieldType.INSTANCE)));

        LLVMTypeRef typeInfoType = LLVMGetElementType(getLlvmType(FTypeType.INSTANCE));
        LLVMValueRef typeInfo = LLVMConstNamedStruct(typeInfoType, createPointerPointer(castedName, castedFields), 2);
        //TODO account for field reorder

        LLVMSetInitializer(res, typeInfo);
    }

    private LLVMValueRef constFieldsForTypeInfo(FClass fClass, Queue<FClass> todo) {
        int fieldsSize = fClass.getInstanceFields().size();

        if (fieldsSize == 0) {
            return constEmptyArray(FArray.getArrayFrom(FFieldType.INSTANCE));
        }

        LLVMValueRef llvmFieldsSize = LLVMConstInt(getLlvmType(FIntN._32), fieldsSize, FALSE);
        PointerPointer<LLVMValueRef> fieldPtrs = createPointerPointer(fClass.getInstanceFields().values(), f -> createFieldInfo(f, todo));
        LLVMValueRef fieldsArray = LLVMConstArray(getLlvmType(FFieldType.INSTANCE), fieldPtrs, fieldsSize);
        LLVMValueRef fields = LLVMConstStructInContext(context, createPointerPointer(llvmFieldsSize, fieldsArray), 2, FALSE);

        LLVMTypeRef fieldsArrayType = LLVMGetElementType(arrayType(FArray.getArrayFrom(FFieldType.INSTANCE), fieldsSize));
        LLVMValueRef res = LLVMAddGlobal(this.module, fieldsArrayType, getTypeInfoFieldsName(fClass));
        setGlobalAttribs(res, Linkage.PRIVATE, true);
        LLVMSetGlobalConstant(res, TRUE);
        LLVMSetInitializer(res, fields);

        return res;
    }

    public LLVMValueRef constEmptyArray(FArray fArray) {
        return LLVMConstBitCast(constEmptyArray, getLlvmType(fArray));
    }

    private LLVMValueRef createFieldInfo(InstanceField field, Queue<FClass> todo) {
        LLVMValueRef name = constantString(field.getIdentifier().name);
        LLVMValueRef castedName = LLVMConstBitCast(name, getLlvmType(FStringLiteral.TYPE));

        //noinspection SuspiciousMethodCalls
        if (!typeInfo.containsKey(field.getType()))
            todo.add((FClass) field.getType());
        LLVMValueRef type = getTypeInfo(field.getType());
        LLVMValueRef memberOf = getTypeInfo(field.getMemberOf());

        //create Global
        LLVMTypeRef fieldInfoType = LLVMGetElementType(getLlvmType(FFieldType.INSTANCE));
        LLVMValueRef res = LLVMAddGlobal(this.module, fieldInfoType, getFieldInfoName(field));
        setGlobalAttribs(res, Linkage.PRIVATE, true);
        LLVMSetGlobalConstant(res, TRUE);
        LLVMValueRef fieldInfo = LLVMConstNamedStruct(fieldInfoType, createPointerPointer(castedName, type, memberOf), 3);
        //TODO account for field reorder

        LLVMSetInitializer(res, fieldInfo);
        return res;
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

    public void link(LLVMModuleRef other) {
        if (LLVMLinkModules2(module, other) != 0)
            Utils.handleError("failed to Link modules");
    }

    //Debug commands

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

    public void emitToFile(LLVMBackend.OutputFileType fileType, String fileName, List<Include> userLibs, boolean debug) { //TODO basically the first two are simple, the latter will need more options like target machine etc.
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
                String tempName = State.get().getTempDir().getPath() + fileName.substring(fileName.lastIndexOf(FileUtils.filesep)) + "_temp.o";
                Log.info(this, "writing temporary object file to: " + tempName);
                try (Target target = Target.getDefault()) {
                    errorId = target.emitToFile(module, tempName, LLVMObjectFile, error);
                    if (errorId != 0)
                        break;
                    ProcessBuilder linkerCall = Linker.buildCall(tempName, fileName, userLibs, target.getTargetTriple(), debug);
                    Log.info(this, "calling Linker: " + Joiners.ON_SPACE.join(linkerCall.command()));
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

package tys.frontier.backend.llvm;

import com.google.common.base.Joiner;
import com.google.common.io.Files;
import com.google.common.primitives.Ints;
import com.koloboke.collect.map.hash.HashObjIntMap;
import com.koloboke.collect.map.hash.HashObjIntMaps;
import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.javacpp.PointerPointer;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.literal.FStringLiteral;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.logging.Log;
import tys.frontier.util.Utils;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.bytedeco.javacpp.LLVM.*;
import static tys.frontier.backend.llvm.LLVMUtil.*;

public class LLVMModule implements AutoCloseable {

    private enum Linkage{
        PRIVATE(LLVMPrivateLinkage),
        EXTERNAL(LLVMExternalLinkage);

        final int type;

        Linkage(int type) {
            this.type = type;
        }

        static Linkage fromVisibility(FVisibilityModifier visibility) { //TODO the goal would be to never generate something others need to link against, so nothing needs external linkage?
            switch (visibility) {
                case EXPORT:
                    return Linkage.EXTERNAL;
                case NONE: case PRIVATE:
                    return Linkage.PRIVATE;
                default:
                    return Utils.cantHappen();
            }
        }
    }

    private static final int CALLING_CONVENTION = LLVMFastCallConv;

    private static final int TRUE = 1;
    private static final int FALSE = 0;

    final LLVMTypeRef byteType;
    final LLVMTypeRef bytePointer;
    final LLVMTypeRef bytePointerPointer;

    LLVMValueRef sfInit;

    private boolean verificationNeeded = false;
    private boolean ownsContext;
    private LLVMContextRef context;
    private LLVMModuleRef module;
    private Map<FType, LLVMTypeRef> llvmTypes = new HashMap<>();
    private Map<String, LLVMValueRef> constantStrings = new HashMap<>();
    private HashObjIntMap<FField> fieldIndices = HashObjIntMaps.newMutableMap();
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

        sfInit = LLVMAddFunction(module, "sf.init", LLVMFunctionType(getLlvmType(FVoid.INSTANCE), (PointerPointer) null, 0, FALSE));
        LLVMAppendBasicBlock(sfInit, "entry");
    }

    private void fillInPredefinedTypes() {
        llvmTypes.put(FBool.INSTANCE, LLVMInt1TypeInContext(context));
        llvmTypes.put(FOptional.from(FBool.INSTANCE), LLVMIntTypeInContext(context, 2));
        llvmTypes.put(FFloat32.INSTANCE, LLVMFloatTypeInContext(context));
        llvmTypes.put(FFloat64.INSTANCE, LLVMDoubleTypeInContext(context));
        llvmTypes.put(FVoid.INSTANCE, LLVMVoidTypeInContext(context));
        llvmTypes.put(FTypeType.INSTANCE, bytePointer);
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
        } else if (fClass instanceof FOptional) {
            res = getLlvmType(((FOptional) fClass).getBaseType());
        } else if (fClass instanceof FFunctionType) {
            res = functionType(((FFunctionType) fClass));
        } else {
            Utils.NYI("LLVM type for: " + fClass);
        }
        llvmTypes.put(fClass, res);
        return res;
    }

    private LLVMTypeRef arrayType(FArray type, int length) {
        PointerPointer<LLVMTypeRef> types = new PointerPointer<>(getLlvmType(FIntN._32),
                LLVMArrayType(getLlvmType(type.getBaseType()), length));
        LLVMTypeRef baseType = LLVMStructTypeInContext(context, types, 2, FALSE);
        return LLVMPointerType(baseType, 0);
    }

    private LLVMTypeRef functionType(FFunctionType functionType) {
        PointerPointer<LLVMTypeRef> params = LLVMUtil.createPointerPointer(functionType.getIn(), this::getLlvmType);
        LLVMTypeRef baseType = LLVMFunctionType(getLlvmType(functionType.getOut()), params, functionType.getIn().size(), FALSE);
        return LLVMPointerType(baseType, 0);
    }

    LLVMValueRef getNull(FOptional fOptional) {
        FType base = fOptional.getBaseType();
        if (base == FBool.INSTANCE) {
            return LLVMConstInt(LLVMIntTypeInContext(context, 2), 2, FALSE);
        } else if (base instanceof FIntN) {
            return LLVMConstInt(getLlvmType(base), ((FIntN) base).minValue().subtract(BigInteger.ONE).longValue(), FALSE);
        } else if (base instanceof FFloat32 || base instanceof FFloat64) {
            return Utils.NYI("null literal for floating point types");
        } else {
            return LLVMConstPointerNull(getLlvmType(base));
        }
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
     * @param classes classes to parse
     */
    public void parseTypes(Iterable<FClass> classes) {
        verificationNeeded = true;
        for (FClass fClass : classes) {
            if (fClass instanceof FPredefinedClass)
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
     * @param classes classes to parse
     */
    public void parseClassMembers(Iterable<FClass> classes) {
        verificationNeeded = true;
        //TODO initializers for fields that are done in the fields
        for (FClass fClass : classes) {

            for (FField field : fClass.getStaticFields().values()) {
                //TODO see if the initializer is a const and direclty init here instead of the block?
                //TODO see if something can be done for final?
                //TODO optimizer flags like we don't care bout the address and readonly
                //TODO for final and effective final fields of objects the pointer pointer could be lowered into a pointer...
                LLVMTypeRef type = getLlvmType(field.getType());
                LLVMValueRef global = LLVMAddGlobal(module, type, getStaticFieldName(field));

                setGlobalAttribs(global, Linkage.fromVisibility(field.getVisibility()), false);
                todoFieldInitilizers.add(field);
            }

            for (FFunction function : fClass.getFunctions().values()) {
                if (!function.isPredefined()) {
                    addFunctionHeader(function);
                    if (!function.isNative())
                        todoFunctionBodies.add(function);
                }
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
        LLVMValueRef res = LLVMAddFunction(module, getFunctionName(function), getLLVMFunctionType(function));
        //set global attribs
        setGlobalAttribs(res, Linkage.fromVisibility(function.getVisibility()), true);
        //LLVMSetFunctionCallConv(res, CALLING_CONVENTION); TODO this crashes the program, but it should work... , maybe its because of the c links ?

        //set names for all arguments, add parameter attributes
        //TODO we can use dereferenceable<i> instead of nonNull, and use dereferenceable_or_null for all others, but we need the type sizes...
        LLVMAttributeRef nonNullAttr = getEnumAttribute("nonnull");
        List<FParameter> fParams = function.getParams();
        for (int i=0; i<fParams.size(); i++) {
            FParameter param = fParams.get(i);
            LLVMSetValueName(LLVMGetParam(res, i), param.getIdentifier().name);
            if (LLVMGetTypeKind(getLlvmType(param.getType())) == LLVMPointerTypeKind && !(param.getType() instanceof FOptional))
                LLVMAddAttributeAtIndex(res, 1+i, nonNullAttr);
        }

        //set return type attributes
        if (function.getType() != FVoid.INSTANCE
                && LLVMGetTypeKind(getLlvmType(function.getType())) == LLVMPointerTypeKind
                && !(function.getType() instanceof FOptional))
            LLVMAddAttributeAtIndex(res, 0, nonNullAttr);
        return res;
    }

    /**
     * @param function function for which we want a LLVM-Function-Type
     * @return the LLVM-Function-Type corresponding to the FFunction
     */
    private LLVMTypeRef getLLVMFunctionType(FFunction function) {
        List<FParameter> fParams = function.getParams();
        PointerPointer<LLVMTypeRef> params = createPointerPointer(function.getParams(), p -> getLlvmType(p.getType()));
        LLVMTypeRef returnType = getLlvmType(function.getType());
        return LLVMFunctionType(returnType, params, fParams.size(), FALSE);
    }

    /**
     * Creates LLVM Code for all parsed Functions and Classes in this module.
     * Should be called after {@link #parseClassMembers}.
     */
    public void fillInBodies() {//TODO consider parallelizing this, but first check how much LLVM likes in module parallelization
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
            //last are bodies for fields
            for (FFunction function : todoFunctionBodies)
                trans.visitFunction(function);
        }
    }

    public void generateMain(FFunction entryPoint) {
        verificationNeeded = true;
        /*
        TODO this is a rough outline on how we would parse input params, but first we need to decide what internal type is string and how to map the i8 array to it
        PointerPointer<LLVMTypeRef> argTypes = new PointerPointer<>(2);
        argTypes.put(0, llvmTypes.get(FInt32.INSTANCE));
        argTypes.put(1, llvmTypes.get(FArray.getArrayFrom(FInt8.INSTANCE, 1)));
        */
        PointerPointer<LLVMTypeRef> argTypes = new PointerPointer<>(0);

        LLVMTypeRef returnType = LLVMInt32Type();
        LLVMTypeRef functionType = LLVMFunctionType(returnType, argTypes, 0, FALSE);

        LLVMValueRef function = LLVMAddFunction(module, "main", functionType);
        LLVMBuilderRef builder = LLVMCreateBuilderInContext(context);
        LLVMBasicBlockRef entryBlock = LLVMAppendBasicBlock(function, "entry");
        LLVMPositionBuilderAtEnd(builder, entryBlock);

        //call entry Point & sfInit
        LLVMBuildCall(builder, sfInit, null, 0, "");
        LLVMValueRef func = LLVMGetNamedFunction(module, getFunctionName(entryPoint));
        LLVMBuildCall(builder, func, null, 0, "");

        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 0, FALSE));

        //end sfInit
        LLVMPositionBuilderAtEnd(builder, LLVMGetEntryBasicBlock(sfInit));
        LLVMBuildRetVoid(builder);
    }

    public void verify() { //TODO this should be called at other places as well
        if (!verificationNeeded)
            return;
        BytePointer outMassage = new BytePointer();
        if (LLVMVerifyModule(module, 1, outMassage) == 1) {
            String s = outMassage.getString();
            LLVMDisposeMessage(outMassage);
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
        verify();
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

    public void emitToFile(LLVMBackend.OutputFileType fileType, String fileName) { //TODO basically the first two are simple, the latter will need more options like target machine etc.
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
                errorId = emitToFile(fileName, LLVMAssemblyFile, error);
                break;
            case NATIVE_OBJECT:
                errorId = emitToFile(fileName, LLVMObjectFile, error);
                break;
            case EXECUTABLE:
                File tempDir = Files.createTempDir();
                String tempName = tempDir.getPath() + fileName.substring(fileName.lastIndexOf(Utils.filesep)) + "_temp.o";
                Log.info(this, "writing temporary object file to: " + tempName);
                errorId = emitToFile(tempName, LLVMObjectFile, error);
                if (errorId == 0) {
                    try {
                        ProcessBuilder linkerCall = Linker.buildCall(tempName, fileName);
                        Log.info(this, "calling Linker: " + Joiner.on(' ').join(linkerCall.command()));
                        Process p = linkerCall.inheritIO().start();
                        p.waitFor();
                    } catch (IOException | InterruptedException e) {
                        Utils.handleException(e);
                    }
                }
                Utils.deleteDir(tempDir);
                break;
        }
        if (errorId != 0) {
            String message = error.getString();
            LLVMDisposeMessage(error);
            Utils.handleError(message);
        }
    }

    //TODO find taget triples and other config options we want to make available and make them params & prolly enum them
    private int emitToFile(String file, int fileType, BytePointer error) {
        LLVMBackend.initialize();
        BytePointer tt = LLVMGetDefaultTargetTriple();
        BytePointer targetTriple = LLVMNormalizeTargetTriple(tt);
        LLVMTargetRef target = new LLVMTargetRef();
        if (LLVMGetTargetFromTriple(targetTriple, target, error) != 0) {
            String message = error.getString();
            LLVMDisposeMessage(error);
            Utils.handleError(message);
        }

        BytePointer cpu = LLVMGetHostCPUName();
        BytePointer features = LLVMGetHostCPUFeatures();
        LLVMTargetMachineRef targetMachine = LLVMCreateTargetMachine(target, targetTriple, cpu, features, LLVMCodeGenLevelAggressive, LLVMRelocDefault, LLVMCodeModelDefault);

        LLVMTargetDataRef dataLayout = LLVMCreateTargetDataLayout(targetMachine);
        LLVMSetModuleDataLayout(module, dataLayout);
        LLVMSetTarget(module, targetTriple);

        int res = LLVMTargetMachineEmitToFile(targetMachine, module, new BytePointer(file), fileType, error);
        LLVMDisposeTargetData(dataLayout);
        LLVMDisposeTargetMachine(targetMachine);
        LLVMDisposeMessage(features);
        LLVMDisposeMessage(cpu);
        LLVMDisposeMessage(targetTriple);
        LLVMDisposeMessage(tt);
        return res;
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

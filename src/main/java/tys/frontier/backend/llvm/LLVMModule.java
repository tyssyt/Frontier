package tys.frontier.backend.llvm;

import com.google.common.collect.ImmutableMap;
import com.google.common.primitives.Ints;
import org.bytedeco.javacpp.BytePointer;
import tys.frontier.code.FClass;
import tys.frontier.code.FFile;
import tys.frontier.code.FFunction;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.predefinedClasses.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.bytedeco.javacpp.LLVM.*;
import static tys.frontier.backend.llvm.LLVMUtil.createPointerPointer;

public class LLVMModule implements AutoCloseable {

    private static final int FALSE = 0;
    private static final int TRUE = 0;

    public static final ImmutableMap<FPredefinedClass, LLVMTypeRef> LLVM_PREDEF_TYPES = ImmutableMap.<FPredefinedClass, LLVMTypeRef>builder()
            .put(FBool.INSTANCE, LLVMInt1Type())
            .put(FInt32.INSTANCE, LLVMInt32Type())
            .put(FInt64.INSTANCE, LLVMInt64Type())
            .put(FFloat32.INSTANCE, LLVMFloatType())
            .put(FFloat64.INSTANCE, LLVMDoubleType())
            .build();

    private boolean verificationNeeded = false;
    private boolean ownsContext;
    private LLVMContextRef context;
    private LLVMModuleRef module;
    private List<FFunction> todoFunctionBodies = new ArrayList<>();
    private Map<FClass, LLVMTypeRef> llvmTypes = new HashMap<>(LLVM_PREDEF_TYPES);

    public LLVMModule(String name) {
        this(name, LLVMGetGlobalContext(), false);
    }

    public LLVMModule(String name, LLVMContextRef context, boolean ownsContext) {
        this.context = context;
        this.module = LLVMModuleCreateWithNameInContext(name, context);
        this.ownsContext = ownsContext;
    }

    @Override
    public void close() {
        LLVMDisposeModule(module);
        if (ownsContext)
            LLVMContextDispose(context);
    }

    LLVMContextRef getContext() {
        return context;
    }

    LLVMModuleRef getModule() {
        return module;
    }

    public Map<FClass, LLVMTypeRef> getLlvmTypes() {
        return llvmTypes;
    }

    /**
     * Gets the LLVM Type corresponding to the FClass.
     * @param fType
     * @return
     */
    public LLVMTypeRef getLLVMType(FClass fType) {
        return llvmTypes.get(fType);
    }

    /**
     * Parses all class types found in the file, creating corresponding LLVM types in this module.
     * @param file file to parse
     */
    public void parseClasses(FFile file) {
        verificationNeeded = true;
        //TODO parse classes, create types and add to llvmTypes
    }

    /**
     * Parses all function Headers found in the file, creating corresponding function prototypes in this module.
     * Should be called after {@Link #parseClasses(FFile)}.
     * @param file
     */
    public void parseFunctionHeaders(FFile file ) {
        verificationNeeded = true;
        //TODO initializers for fields that are done in the fields
        for (FClass clazz : file.getClasses().values())
            for (FFunction function : clazz.getFunctions().values())
                addFunction(function);
    }

    /**
     * Parses function header and adds a Prototype to this module.
     * Does not generate code for the body.
     * @param function
     * @return Reference to the LLVM function
     */
    private LLVMValueRef addFunction(FFunction function) {
        LLVMValueRef res = LLVMAddFunction(module, function.getIdentifier().name, getLLVMFunctionType(function));
        //set names for all arguments
        int offset = 0;
        if (!function.isStatic()) {
            LLVMSetValueName(LLVMGetParam(res, 0), "this");
            offset = 1;
        }
        List<FLocalVariable> fParams = function.getParams();
        for (int i=0; i<fParams.size(); i++)
            LLVMSetValueName(LLVMGetParam(res, i+offset), fParams.get(i).getIdentifier().name);
        todoFunctionBodies.add(function);
        return res;
    }

    /**
     * @param function function for which we want a LLVM-Function-Type
     * @return the LLVM-Function-Type corresponding to the FFunction
     */
    private LLVMTypeRef getLLVMFunctionType(FFunction function) {
        List<FLocalVariable> fParams = function.getParams();
        int size = fParams.size();
        if (!function.isStatic())
            size++;

        List<LLVMTypeRef> llvmParamTypes = new ArrayList<>(size);
        if (!function.isStatic())
            llvmParamTypes.add(llvmTypes.get(function.getClazz())); //add this as first param
        for (FLocalVariable param : fParams)
            llvmParamTypes.add(llvmTypes.get(param.getType()));

        LLVMTypeRef returnType = getLLVMType(function.getType());
        return LLVMFunctionType(returnType, createPointerPointer(llvmParamTypes), size, FALSE);
    }

    /**
     * Creates LLVM Code for all parsed Functions in this module.
     * Should be called after {@Link #parseFunctionHeaders(FFile)}.
     */
    public void fillInFunctionBodies() {//TODO consider parallelizing this, but first check how much LLVM likes in module parallelization
        verificationNeeded = true;
        try (LLVMTransformer trans = new LLVMTransformer(this)) {
            for (FFunction function : todoFunctionBodies) {
                trans.visitFunction(function);
            }
        }
        verify();
    }

    public void verify() {
        if (!verificationNeeded)
            return;
        BytePointer outMassage = new BytePointer();
        if (LLVMVerifyModule(module, 1, outMassage) == 1) {
            RuntimeException e = new RuntimeException(outMassage.getString());
            LLVMDisposeMessage(outMassage);
            throw e; //TODO error handling
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

    public void dump() {
        LLVMDumpModule(module);
    }

    public void viewFunctionControlFlowGraph(FFunction function) {
        LLVMValueRef res = LLVMGetNamedFunction(module, function.getIdentifier().name);
        if (res.isNull())
            return;
        LLVMViewFunctionCFG(res);
        //LLVMViewFunctionCFGOnly(res);
    }


}

package tys.frontier.backend.llvm;

import com.google.common.primitives.Ints;
import com.koloboke.collect.map.hash.HashObjIntMap;
import com.koloboke.collect.map.hash.HashObjIntMaps;
import org.bytedeco.javacpp.BytePointer;
import tys.frontier.code.*;
import tys.frontier.code.predefinedClasses.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.bytedeco.javacpp.LLVM.*;
import static tys.frontier.backend.llvm.LLVMUtil.*;

public class LLVMModule implements AutoCloseable {

    private static final int FALSE = 0;
    private static final int TRUE = 0;

    private boolean verificationNeeded = false;
    private boolean ownsContext;
    private LLVMContextRef context;
    private LLVMModuleRef module;
    private Map<FClass, LLVMTypeRef> llvmTypes = new HashMap<>();
    private HashObjIntMap<FField> fieldIndices = HashObjIntMaps.newMutableMap();
    private List<FClass> todoTypeBodies = new ArrayList<>();
    private List<FFunction> todoFunctionBodies = new ArrayList<>();

    public LLVMModule(String name) {
        this(name, LLVMGetGlobalContext(), false);
    }

    public LLVMModule(String name, LLVMContextRef context, boolean ownsContext) {
        this.context = context;
        this.module = LLVMModuleCreateWithNameInContext(name, context);
        this.ownsContext = ownsContext;
        fillInPredefinedTypes();
    }

    private void fillInPredefinedTypes() {
        llvmTypes.put(FBool.INSTANCE, LLVMInt1TypeInContext(context));
        llvmTypes.put(FInt32.INSTANCE, LLVMInt32TypeInContext(context));
        llvmTypes.put(FInt64.INSTANCE, LLVMInt64TypeInContext(context));
        llvmTypes.put(FFloat32.INSTANCE, LLVMFloatTypeInContext(context));
        llvmTypes.put(FFloat64.INSTANCE, LLVMDoubleTypeInContext(context));
        llvmTypes.put(FVoid.INSTANCE, LLVMVoidTypeInContext(context));
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

    Map<FClass, LLVMTypeRef> getLlvmTypes() {
        return llvmTypes;
    }

    HashObjIntMap<FField> getFieldIndices() {
        return fieldIndices;
    }

    public int getFieldIndex(FField field) {
        return fieldIndices.getInt(field);
    }

    /**
     * Parses all class types found in the file, creating corresponding LLVM types in this module.
     * @param file file to parse
     */
    public void parseTypes(FFile file) {
        verificationNeeded = true;
        for (FClass clazz : file.getClasses().values()) {
            if (clazz instanceof FPredefinedClass)
                continue;
            LLVMTypeRef baseType = LLVMStructCreateNamed(context, "class." + clazz.getIdentifier().name);
            LLVMTypeRef pointerType = LLVMPointerType(baseType, 0);
            LLVMTypeRef old = llvmTypes.put(clazz, pointerType);
            if (old != null)
                throw new RuntimeException("type defined twice:" + clazz.getIdentifier());
            todoTypeBodies.add(clazz);
        }
    }

    /**
     * Parses all function Headers found in the file, creating corresponding function prototypes in this module.
     * Should be called after {@link #parseTypes(FFile)}.
     * @param file input file
     */
    public void parseClassMembers(FFile file ) {
        verificationNeeded = true;
        //TODO initializers for fields that are done in the fields
        for (FClass clazz : file.getClasses().values()) {
            if (clazz instanceof FPredefinedClass)
                continue;
            for (FField field : clazz.getFields().values()) {
                if (field.isStatic()) {
                    //TODO see if the initializer is a const and direclty init here instead of the block?
                    //TODO see if something can be done for final?
                    //TODO optimizer flags like we don't care bout the address and readonly
                    //TODO for final and effective final fields of objects the pointer pointer could be lowered into a pointer...
                    LLVMTypeRef type = llvmTypes.get(field.getType());
                    LLVMValueRef global = LLVMAddGlobal(module, type, getStaticFieldName(field));
                    LLVMSetInitializer(global, LLVMConstNull(type));
                }
            }
            for (FFunction function : clazz.getFunctions().values()) {
                if (function.isPredefined())
                    continue;
                addFunction(function);
            }
        }
    }

    /**
     * Parses function header and adds a Prototype to this module.
     * Does not generate code for the body.
     * @param function function to add
     * @return Reference to the LLVM function
     */
    private LLVMValueRef addFunction(FFunction function) {
        LLVMValueRef res = LLVMAddFunction(module, getFunctionName(function), getLLVMFunctionType(function));
        //set names for all arguments
        int offset = 0;
        if (!function.isStatic()) {
            LLVMSetValueName(LLVMGetParam(res, 0), "this");
            offset = 1;
        }
        List<FLocalVariable> fParams = function.getParams();
        for (int i=0; i<fParams.size(); i++)
            LLVMSetValueName(LLVMGetParam(res, i + offset), fParams.get(i).getIdentifier().name);
        if (!function.isPredefined())
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
            llvmParamTypes.add(llvmTypes.get(function.getClazz())); //add 'this' as first param
        for (FLocalVariable param : fParams)
            llvmParamTypes.add(llvmTypes.get(param.getType()));

        LLVMTypeRef returnType = llvmTypes.get(function.getType());
        return LLVMFunctionType(returnType, createPointerPointer(llvmParamTypes), size, FALSE);
    }

    /**
     * Creates LLVM Code for all parsed Functions and Classes in this module.
     * Should be called after {@link #parseClassMembers(FFile)}.
     */
    public void fillInBodies() {//TODO consider parallelizing this, but first check how much LLVM likes in module parallelization
        verificationNeeded = true;

        //start with filling in the bodies for missing types
        for (FClass type : todoTypeBodies) {
            List<LLVMTypeRef> subtypes = new ArrayList<>();
            int index = 0;
            for (FField field : type.getFields().values()) {
                if (field.isStatic())
                    continue;
                subtypes.add(llvmTypes.get(field.getType()));
                fieldIndices.put(field, index++);
            }
            LLVMStructSetBody(LLVMGetElementType(llvmTypes.get(type)), createPointerPointer(subtypes), subtypes.size(), FALSE);
        }

        try (LLVMTransformer trans = new LLVMTransformer(this)) {
            //last are bodies for fields
            for (FFunction function : todoFunctionBodies)
                trans.visitFunction(function);
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

package tys.frontier.backend.llvm;

import com.google.common.primitives.Ints;
import com.koloboke.collect.map.hash.HashObjIntMap;
import com.koloboke.collect.map.hash.HashObjIntMaps;
import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.javacpp.PointerPointer;
import tys.frontier.code.*;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.*;

import java.io.IOException;
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


        //TODO debug again
        LLVMTypeRef intType = LLVMInt32TypeInContext(context);
        LLVMTypeRef functiontype = LLVMFunctionType(intType, intType, 1, FALSE);
        LLVMAddFunction(module, "putchar", functiontype);
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

    public void parseDependencies(Module fModule) {
        verificationNeeded = true;
        for (FClass clazz : fModule.getImportedClasses().values()) {
            assert !(clazz instanceof FPredefinedClass);
            parseType(clazz);
        }

        for (FClass clazz : fModule.getImportedClasses().values()) {
            for (FField field : clazz.getFields().values()) {
                if (field.isStatic()) {
                    LLVMTypeRef type = llvmTypes.get(field.getType());
                    LLVMAddGlobal(module, type, getStaticFieldName(field));
                }
            }
            for (FFunction function : clazz.getFunctions().values()) {
                assert !function.isPredefined();
                addFunctionHeader(function);
            }
        }
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
            parseType(clazz);
            todoTypeBodies.add(clazz);
        }
    }

    private void parseType(FClass clazz) {
        LLVMTypeRef baseType = LLVMStructCreateNamed(context, "class." + clazz.getIdentifier().name);
        LLVMTypeRef pointerType = LLVMPointerType(baseType, 0);
        LLVMTypeRef old = llvmTypes.put(clazz, pointerType);
        if (old != null)
            throw new RuntimeException("type defined twice:" + clazz.getIdentifier());
    }

    /**
     * Parses all function Headers found in the file, creating corresponding function prototypes in this module.
     * Should be called after {@link #parseTypes(FFile)}.
     * @param file input file
     */
    public void parseClassMembers(FFile file) {
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
                addFunctionHeader(function);
                todoFunctionBodies.add(function);
            }
        }
    }

    /**
     * Parses function header and adds a Prototype to this module.
     * Does not generate code for the body.
     * @param function function to add
     * @return Reference to the LLVM function
     */
    private LLVMValueRef addFunctionHeader(FFunction function) {
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

        //call entry Point
        LLVMValueRef func = LLVMGetNamedFunction(module, getFunctionName(entryPoint));
        LLVMBuildCall(builder, func, null, 0, "callMainEntryPoint");

        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 0, FALSE));
    }

    public void verify() { //TODO this should be called at other places as well
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
                String tempName = fileName + "_temp.o";
                errorId = emitToFile(tempName, LLVMObjectFile, error); //TODO dirty hacks :D
                if (errorId == 0){
                    try {
                        Process p = Linker.buildCall(tempName, fileName).inheritIO().start();
                        p.waitFor();
                    } catch (IOException | InterruptedException e) {
                        throw new RuntimeException(e); //TODO error handling
                    }
                }
                break;
        }
        if (errorId != 0) {
            String message = error.getString();
            LLVMDisposeMessage(error);
            throw new RuntimeException(message); //TODO error handling
        }
    }

    //TODO find taget triples and other config options we want to make available and make them params & prolly enum them
    private int emitToFile(String file, int fileType, BytePointer error) {
        LLVMBackend.initialize();
        BytePointer targetTriple = LLVMGetDefaultTargetTriple();
        LLVMTargetRef target = new LLVMTargetRef();
        if (LLVMGetTargetFromTriple(targetTriple, target, error) != 0) {
            RuntimeException e = new RuntimeException(error.getString());
            LLVMDisposeMessage(error);
            throw new RuntimeException(e); //TODO error handling;
        }
        String cpu = "generic"; //TODO is there any other useful value here?
        LLVMTargetMachineRef targetMachine = LLVMCreateTargetMachine(target, targetTriple.getString(), cpu, "", LLVMCodeGenLevelAggressive, LLVMRelocDefault, LLVMCodeModelDefault);

        LLVMTargetDataRef dataLayout = LLVMCreateTargetDataLayout(targetMachine);
        LLVMSetModuleDataLayout(module, dataLayout);
        LLVMSetTarget(module, targetTriple);

        int res = LLVMTargetMachineEmitToFile(targetMachine, module, new BytePointer(file), LLVMObjectFile, error);
        LLVMDisposeTargetData(dataLayout);
        LLVMDisposeTargetMachine(targetMachine);
        LLVMDisposeMessage(targetTriple);
        return res;
    }

}

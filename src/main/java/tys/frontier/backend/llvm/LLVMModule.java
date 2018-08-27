package tys.frontier.backend.llvm;

import com.google.common.primitives.Ints;
import com.koloboke.collect.map.hash.HashObjIntMap;
import com.koloboke.collect.map.hash.HashObjIntMaps;
import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.javacpp.PointerPointer;
import tys.frontier.code.*;
import tys.frontier.code.literal.FStringLiteral;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.modules.io.IOClass;
import tys.frontier.util.Utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.google.common.collect.Iterables.getOnlyElement;
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
        fillInPredefinedTypes();
    }

    private void fillInPredefinedTypes() {
        llvmTypes.put(FBool.INSTANCE, LLVMInt1TypeInContext(context));
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

    LLVMModuleRef getModule() {
        return module;
    }

    LLVMBuilderRef createBuilder() {
        return LLVMCreateBuilderInContext(this.context);
    }

    LLVMTypeRef getLlvmType (FType fType) { //TODO needs sync for multithreading
        LLVMTypeRef res = llvmTypes.get(fType);
        if (res != null) {
            return res;
        } else if (fType instanceof FInterface) {
            //TODO return anonymous struct type with one vtable pointer that is the same for all interfaces
            Utils.NYI("LLVM type for Interfaces");
        } else if (fType instanceof FIntN) {
            res = LLVMIntTypeInContext(context, ((FIntN) fType).getN());
        } else if (fType instanceof FArray) {
            res = arrayType(((FArray) fType), 0);
        } else {
            Utils.NYI("LLVM type for: " + fType);
        }
        llvmTypes.put(fType, res);
        return res;
    }

    private LLVMTypeRef arrayType(FArray type, int length) {
        PointerPointer<LLVMTypeRef> types = new PointerPointer<>(getLlvmType(FIntN._32),
                LLVMArrayType(getLlvmType(type.getOneDimensionLess()), length));
        LLVMTypeRef baseType = LLVMStructTypeInContext(context, types, 2, FALSE);
        return LLVMPointerType(baseType, 0);
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

    public void parseDependencies(Module fModule) {
        verificationNeeded = true;
        for (FType fType : fModule.getImportedClasses().values()) {
            //assert !(fType instanceof FPredefinedClass);
            if (fType instanceof FPredefinedClass || !(fType instanceof FClass)) //TODO this is a hack that needs to stay until we have binary modules
                continue;
            parseClass(((FClass) fType));
        }

        for (FType fType : fModule.getImportedClasses().values()) {
            if (fType instanceof FPredefinedClass || !(fType instanceof FClass)) //TODO this is a hack that needs to stay until we have binary modules
                continue;
            for (FField field : fType.getStaticFields().values()) {
                LLVMTypeRef type = getLlvmType(field.getType());
                LLVMAddGlobal(module, type, getStaticFieldName(field));
            }
            for (FFunction function : fType.getFunctions()) {
                assert !function.isPredefined();
                addFunctionHeader(function);
            }
        }

        //FIXME the hack part:
        for (FType clazz : fModule.getImportedClasses().values()) {
            if (clazz == IOClass.INSTANCE) {
                LLVMAddFunction(module, "putchar", getLLVMFunctionType(getOnlyElement(clazz.getStaticFunctions(IOClass.PUTCHAR_ID))));
                LLVMAddFunction(module, "getchar", getLLVMFunctionType(getOnlyElement(clazz.getStaticFunctions(IOClass.GETCHAR_ID))));
            }
        }

    }

    /**
     * Parses all class types found in the file, creating corresponding LLVM types in this module.
     * @param file file to parse
     */
    public void parseTypes(FFile file) {
        verificationNeeded = true;
        for (FType fType : file.getTypes().values()) {
            if (fType instanceof FPredefinedClass || !(fType instanceof FClass))
                continue;
            parseClass(((FClass) fType));
            todoClassBodies.add(((FClass) fType));
        }
    }

    private void parseClass(FClass clazz) {
        LLVMTypeRef baseType = LLVMStructCreateNamed(context, getClassName(clazz));
        LLVMTypeRef pointerType = LLVMPointerType(baseType, 0);
        LLVMTypeRef old = llvmTypes.put(clazz, pointerType);
        if (old != null)
            Utils.cantHappen();
    }

    /**
     * Parses all function Headers found in the file, creating corresponding function prototypes in this module.
     * Should be called after {@link #parseTypes(FFile)}.
     * @param file input file
     */
    public void parseClassMembers(FFile file) {
        verificationNeeded = true;
        //TODO initializers for fields that are done in the fields
        for (FType fType : file.getTypes().values()) {
            if (fType instanceof FPredefinedClass)
                continue;
            if (fType instanceof FClass) {
                for (FField field : fType.getStaticFields().values()) {
                    if (field.isStatic()) {
                        //TODO see if the initializer is a const and direclty init here instead of the block?
                        //TODO see if something can be done for final?
                        //TODO optimizer flags like we don't care bout the address and readonly
                        //TODO for final and effective final fields of objects the pointer pointer could be lowered into a pointer...
                        LLVMTypeRef type = getLlvmType(field.getType());
                        LLVMValueRef global = LLVMAddGlobal(module, type, getStaticFieldName(field));

                        setGlobalAttribs(global, Linkage.fromVisibility(field.getVisibility()), false);
                        //LLVMSetGlobalConstant(global, whoKnows); TODO find out if it is constant
                        todoFieldInitilizers.add(field);
                    }
                }
            }
            for (FFunction function : fType.getFunctions()) {
                if (function.isPredefined() || function.isAbstract())
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
        //set global attribs
        setGlobalAttribs(res, Linkage.fromVisibility(function.getVisibility()), true);
        //LLVMSetFunctionCallConv(res, CALLING_CONVENTION); TODO this crashes the program, but it should work...

        //set names for all arguments
        int offset = 0;
        if (!function.isStatic()) {
            LLVMSetValueName(LLVMGetParam(res, 0), "this");
            offset = 1;
        }
        List<FParameter> fParams = function.getParams();
        for (int i=0; i<fParams.size(); i++)
            LLVMSetValueName(LLVMGetParam(res, i + offset), fParams.get(i).getIdentifier().name);
        return res;
    }

    /**
     * @param function function for which we want a LLVM-Function-Type
     * @return the LLVM-Function-Type corresponding to the FFunction
     */
    private LLVMTypeRef getLLVMFunctionType(FFunction function) {
        function = function.getRootDefinition(); //this changes the "this" type to that of the root definition
        List<FParameter> fParams = function.getParams();
        int size = fParams.size();
        if (!function.isStatic())
            size++;

        List<LLVMTypeRef> llvmParamTypes = new ArrayList<>(size);
        if (!function.isStatic())
            llvmParamTypes.add(getLlvmType(function.getMemberOf())); //add 'this' as first param
        for (FLocalVariable param : fParams)
            llvmParamTypes.add(getLlvmType(param.getType()));

        LLVMTypeRef returnType = getLlvmType(function.getType());
        return LLVMFunctionType(returnType, createPointerPointer(llvmParamTypes), size, FALSE);
    }

    /**
     * Creates LLVM Code for all parsed Functions and Classes in this module.
     * Should be called after {@link #parseClassMembers(FFile)}.
     */
    public void fillInBodies() {//TODO consider parallelizing this, but first check how much LLVM likes in module parallelization
        verificationNeeded = true;

        //start with filling in the bodies for missing types
        for (FClass fClass : todoClassBodies) {
            List<LLVMTypeRef> subtypes = new ArrayList<>();
            int index = 0;
            if (fClass.getSuperClass() != null) { //FIXME once we have always superclasses, this check is no longer necessary
                subtypes.add(LLVMGetElementType(getLlvmType(fClass.getSuperClass())));
            }
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

        //call entry Point
        LLVMValueRef func = LLVMGetNamedFunction(module, getFunctionName(entryPoint));
        LLVMBuildCall(builder, func, null, 0, "");

        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 0, FALSE));
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
                String tempName = fileName + "_temp.o";
                errorId = emitToFile(tempName, LLVMObjectFile, error); //TODO dirty hacks :D
                if (errorId == 0){
                    try {
                        Process p = Linker.buildCall(tempName, fileName).inheritIO().start();
                        p.waitFor();
                    } catch (IOException | InterruptedException e) {
                        Utils.handleException(e);
                    }
                }
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
        BytePointer targetTriple = LLVMGetDefaultTargetTriple();
        LLVMTargetRef target = new LLVMTargetRef();
        if (LLVMGetTargetFromTriple(targetTriple, target, error) != 0) {
            String message = error.getString();
            LLVMDisposeMessage(error);
            Utils.handleError(message);
        }
        String cpu = "generic"; //TODO is there any other useful value here?
        LLVMTargetMachineRef targetMachine = LLVMCreateTargetMachine(target, targetTriple.getString(), cpu, "", LLVMCodeGenLevelAggressive, LLVMRelocDefault, LLVMCodeModelDefault);

        LLVMTargetDataRef dataLayout = LLVMCreateTargetDataLayout(targetMachine);
        LLVMSetModuleDataLayout(module, dataLayout);
        LLVMSetTarget(module, targetTriple);

        int res = LLVMTargetMachineEmitToFile(targetMachine, module, new BytePointer(file), fileType, error);
        LLVMDisposeTargetData(dataLayout);
        LLVMDisposeTargetMachine(targetMachine);
        LLVMDisposeMessage(targetTriple);
        return res;
    }

    private void setGlobalAttribs(LLVMValueRef global, Linkage linkage, boolean unnamedAddr) {
        LLVMSetLinkage(global, linkage.type);
        LLVMSetUnnamedAddr(global, unnamedAddr ? TRUE : FALSE);
    }

}

package tys.frontier.backend.llvm;

import org.bytedeco.javacpp.BytePointer;
import tys.frontier.backend.Backend;
import tys.frontier.code.FFile;

import java.util.Collection;
import java.util.Collections;

import static org.bytedeco.javacpp.LLVM.*;

public class LLVMBackend implements Backend {

    private static boolean initialized = false;

    private static void initialize() {
        if (initialized)
            return;
        initialized = true;

        LLVMInitializeAllTargetInfos();
        LLVMInitializeAllTargets();
        LLVMInitializeAllTargetMCs();
        LLVMInitializeAllAsmParsers();
        LLVMInitializeAllAsmPrinters();
    }

    public static void runBackend(FFile file, String out) {
        //TODO a pass that transformes for each into for
        //TODO a pass that creates init function from all field initializers and appends it to constructors
        try (LLVMModule module = createModule(file)) {
            module.optimize(3);
            emitToFile(module, out);
        }
    }

    public static LLVMModule createModule(FFile file) {
        return createModule(Collections.singleton(file), file.getName());
    }

    public static LLVMModule createModule(Collection<FFile> files, String name) {
        LLVMModule res = new LLVMModule(name);
        for (FFile file : files)
            res.parseClasses(file);
        for (FFile file : files)
            res.parseFunctionHeaders(file);
        res.fillInBodies();
        return res;
    }

    //TODO find taget triples and other config options we want to make available and make them params & prolly enum them
    public static void emitToFile(LLVMModule module, String file) {
        initialize();
        BytePointer targetTriple = LLVMGetDefaultTargetTriple();
        BytePointer error = new BytePointer();
        LLVMTargetRef target = new LLVMTargetRef();
        if (LLVMGetTargetFromTriple(targetTriple, target, error) != 0) {
            RuntimeException e = new RuntimeException(error.getString());
            LLVMDisposeMessage(error);
            throw new RuntimeException(e); //TODO error handling;
        }
        String cpu = "generic"; //TODO is there any other useful value here?
        LLVMTargetMachineRef targetMachine = LLVMCreateTargetMachine(target, targetTriple.getString(), cpu, "", LLVMCodeGenLevelAggressive, LLVMRelocDefault, LLVMCodeModelDefault);

        LLVMTargetDataRef dataLayout = LLVMCreateTargetDataLayout(targetMachine);
        LLVMSetModuleDataLayout(module.getModule(), dataLayout);
        LLVMSetTarget(module.getModule(), targetTriple);

        if (LLVMTargetMachineEmitToFile(targetMachine, module.getModule(), new BytePointer(file), LLVMObjectFile, error) != 0) {
            RuntimeException e = new RuntimeException(error.getString());
            LLVMDisposeMessage(error);
            throw e; //TODO error handling
        }
        LLVMDisposeTargetData(dataLayout);
        LLVMDisposeTargetMachine(targetMachine);
        LLVMDisposeMessage(targetTriple);
    }

}

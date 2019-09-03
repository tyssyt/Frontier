package tys.frontier.backend.llvm;

import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.javacpp.PointerPointer;
import tys.frontier.util.Utils;

import static org.bytedeco.javacpp.LLVM.*;

public class Target implements AutoCloseable {

    private static boolean initialized = false;
    synchronized static void initialize() {
        if (initialized)
            return;
        initialized = true;
        LLVMInitializeAllTargetInfos();
        LLVMInitializeAllTargets();
        LLVMInitializeAllTargetMCs();
        LLVMInitializeAllAsmParsers();
        LLVMInitializeAllAsmPrinters();
    }

    private String targetTriple;
    private LLVMTargetRef targetRef;
    private String cpu;
    private String features;
    private LLVMTargetMachineRef targetMachine;
    private LLVMTargetDataRef dataLayout;

    public static Target getDefault() {
        BytePointer tt = LLVMGetDefaultTargetTriple();
        BytePointer targetTriple = LLVMNormalizeTargetTriple(tt);
        BytePointer cpu = LLVMGetHostCPUName();
        BytePointer features = LLVMGetHostCPUFeatures();
        Target res = new Target(targetTriple.getString(), cpu.getString(), features.getString());
        LLVMDisposeMessage(features);
        LLVMDisposeMessage(cpu);
        LLVMDisposeMessage(targetTriple);
        LLVMDisposeMessage(tt);
        return res;
    }

    public Target(String targetTriple, String cpu, String features) {
        initialize();

        BytePointer error = new BytePointer();

        this.targetTriple = targetTriple;

        PointerPointer<LLVMTargetRef> pp = new PointerPointer<>(1);
        if (LLVMGetTargetFromTriple(targetTriple, pp, error) != 0) {
            String message = error.getString();
            LLVMDisposeMessage(error);
            Utils.handleError(message);
        }
        targetRef = pp.get(LLVMTargetRef.class);

        this.cpu = cpu;
        this.features = features;

        this.targetMachine = LLVMCreateTargetMachine(targetRef, targetTriple, cpu, features, LLVMCodeGenLevelAggressive, LLVMRelocDefault, LLVMCodeModelDefault);
        this.dataLayout = LLVMCreateTargetDataLayout(targetMachine);
    }

    @Override
    public void close() {
        LLVMDisposeTargetData(dataLayout);
        LLVMDisposeTargetMachine(targetMachine);
    }

    public int emitToFile(LLVMModuleRef module, String file, int fileType, BytePointer error) {
        LLVMSetModuleDataLayout(module, dataLayout);
        LLVMSetTarget(module, targetTriple);
        return LLVMTargetMachineEmitToFile(targetMachine, module, new BytePointer(file), fileType, error);
    }

    public String getTargetTriple() {
        return targetTriple;
    }

    public LLVMTargetRef getTargetRef() {
        return targetRef;
    }

    public String getCpu() {
        return cpu;
    }

    public String getFeatures() {
        return features;
    }

    public LLVMTargetMachineRef getTargetMachine() {
        return targetMachine;
    }

    public LLVMTargetDataRef getDataLayout() {
        return dataLayout;
    }
}

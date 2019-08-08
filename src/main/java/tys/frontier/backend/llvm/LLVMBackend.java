package tys.frontier.backend.llvm;

import tys.frontier.backend.Backend;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.module.Module;
import tys.frontier.code.type.FClass;
import tys.frontier.passes.analysis.reachability.Reachability;

import java.util.ArrayList;

import static org.bytedeco.javacpp.LLVM.*;

public class LLVMBackend implements Backend {

    public  enum OutputFileType { //TODO this will change, see the todo in tofile of Module
        LLVM_IR("ll"),
        LLVM_BITCODE("bc"),
        TEXTUAL_ASSEMBLY("s"), //TODO from here on content is taget dependend
        NATIVE_OBJECT("o"),
        EXECUTABLE("exe"); //TODO here even the extension is target dependend

        public final String fileExtension;

        OutputFileType(String fileExtension) {
            this.fileExtension = fileExtension;
        }
    }

    //TODO see if LTO is anything worth investing time into

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

    public static void runBackend(Module fModule, Reachability reachability, String out, OutputFileType fileType) {
        Iterable<FClass> classes;
        if (reachability == null) {
            classes = new ArrayList<>();
            for (Module m : fModule.getImportedModulesReflexiveTransitive()) {
                ((ArrayList<FClass>) classes).addAll(m.getClasses().values());
            }
        } else {
            classes = reachability.getReachableClasses().keySet();
        }
        //TODO a pass that creates init function from all field initializers and appends it to constructors
        //TODO optimization oppertunity, when a param is never written to (or dereferenced) we don't have to alloca it... but that can be done by opt passes...
        try (LLVMModule module = createModule(fModule.getName(), classes, fModule.getEntryPoint().orElse(null))) {
            if (out.indexOf('.') == -1)
                out = out + '.' + fileType.fileExtension;
            if (fileType == OutputFileType.LLVM_IR) {
                module.emitToFile(fileType, out);
                return;
            }
            System.out.println("generated Module: " + module.emitToString());
            module.optimize(3);
            //System.out.println("optimized Module: " + module.emitToString());
            module.emitToFile(fileType, out);
        }
    }

    public static LLVMModule createModule(String name, Iterable<FClass> classes, FFunction entryPoint) {
        LLVMModule res = new LLVMModule(name);
        res.parseTypes(classes);
        res.parseClassMembers(classes);
        res.fillInBodies();
        if (entryPoint != null)
            res.generateMain(entryPoint);
        return res;
    }

}

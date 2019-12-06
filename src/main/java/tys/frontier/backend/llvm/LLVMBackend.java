package tys.frontier.backend.llvm;

import tys.frontier.backend.Backend;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.module.Module;
import tys.frontier.code.type.FClass;
import tys.frontier.passes.analysis.reachability.Reachability;

import static java.util.stream.Collectors.toList;

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

    public static void runBackend(Module fModule, Reachability reachability, String out, OutputFileType fileType) {
        Iterable<FClass> classes;
        if (reachability == null) {
            classes = fModule.findImportedModulesReflexiveTransitive().stream()
                    .flatMap(Module::getClasses)
                    .collect(toList());
        } else {
            classes = reachability.getReachableClasses().keySet();
        }
        //TODO a pass that creates init function from all field initializers and appends it to constructors
        //TODO optimization oppertunity, when a param is never written to (or dereferenced) we don't have to alloca it... but that can be done by opt passes...
        try (LLVMModule module = createModule(fModule.getEntryPoint().getFileName(), classes, fModule.findMain())) {
            if (out.lastIndexOf('.') < 2) //TODO this breaks if .. appears in out
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

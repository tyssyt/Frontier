package tys.frontier.backend.llvm;

import tys.frontier.backend.Backend;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.module.FrontierModule;
import tys.frontier.code.module.Module;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.passes.analysis.reachability.Reachability;

import java.util.Collection;
import java.util.List;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

public class LLVMBackend implements Backend {

    public enum OutputFileType { //TODO this will change, see the todo in tofile of Module
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

    public static void runBackend(FrontierModule fModule, Reachability reachability, String out, OutputFileType fileType, boolean debug) {
        Collection<DefaultNamespace> namespaces;
        List<Module> allModules = fModule.findImportedModulesReflexiveTransitive();
        if (reachability == null) {
            namespaces = allModules.stream()
                    .flatMap(Module::getNamespaces)
                    .collect(toList());
        } else {
            namespaces = reachability.getReachableNamespaces().keySet();
        }
        //TODO a pass that creates init function from all field initializers and appends it to constructors
        //TODO optimization oppertunity, when a param is never written to (or dereferenced) we don't have to alloca it... but that can be done by opt passes...
        try (LLVMModule module = createModule(fModule.getEntryPoint().getFilePath().toString(), namespaces, fModule.findMain())) {
            if (out.lastIndexOf('.') < 2) //TODO this breaks if .. appears in out
                out = out + '.' + fileType.fileExtension;
            if (fileType == OutputFileType.LLVM_IR) {
                module.emitToFile(fileType, out, emptyList(), debug);
                return;
            }
            System.out.println("generated Module: " + module.emitToString());
            module.verify();
            //module.optimize(3); //TODO see the BreaksOptimizer test for why we need to disable optimization
            //System.out.println("optimized Module: " + module.emitToString());
            module.emitToFile(fileType, out, allModules.stream().flatMap(m -> m.getNativeIncludes().stream()).collect(toList()), debug);
        }
    }

    public static LLVMModule createModule(String name, Collection<DefaultNamespace> namespaces, FFunction entryPoint) {
        LLVMModule res = new LLVMModule(name);
        res.parseTypes(namespaces);
        res.parseClassMembers(namespaces);
        res.fillInBodies(namespaces, entryPoint);
        res.createMetaData();
        return res;
    }

}

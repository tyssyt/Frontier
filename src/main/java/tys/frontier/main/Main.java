package tys.frontier.main;

import com.google.common.io.Files;
import tys.frontier.State;
import tys.frontier.backend.llvm.LLVMBackend;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FInstantiatedFunction;
import tys.frontier.code.module.Module;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.parser.Parser;
import tys.frontier.parser.modules.FolderRepository;
import tys.frontier.parser.modules.ImportResolver;
import tys.frontier.parser.modules.ModuleRepository;
import tys.frontier.parser.modules.ResourceRepository;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.passes.lowering.FForEachLowering;
import tys.frontier.passes.lowering.FLambdaIfLowering;
import tys.frontier.style.Style;
import tys.frontier.util.FileUtils;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public class Main {

    private static final LLVMBackend.OutputFileType outputType =
            LLVMBackend.OutputFileType.EXECUTABLE;

    public static void main(String[] args) throws SyntaxError, SyntaxErrors, IOException {
        //TODO use some library for argument parsing?
        String input;
        String output = null;
        List<ModuleRepository> repositories = new ArrayList<>();
        boolean keepTmpDir = false;

        int i = 0;
        while (i < args.length) {
            String arg = args[i];
            if (arg.equals("--")) {
                i++;
                break;
            }

            switch (arg) {
                case "-out":
                    i++;
                    if (output != null)
                        System.err.println("-out argument given more then once");
                    output = args[i];
                    break;
                case "-folderRepository":
                    i++;
                    Path path = Paths.get(args[i]);
                    repositories.add(new FolderRepository(path, Style.DEFAULT_STYLE)); //TODO find a way to specify style here
                    break;
                case "-keepTmpDir":
                    i++;
                    keepTmpDir = Boolean.parseBoolean(args[i]);
                    break;
                default:
                    System.err.println("unrecognized argument: " + arg);
                    break;
            }
            i++;
        }

        if (i < args.length)
            input = args[i];
        else {
            System.err.println("no input file");
            return;
        }

        if (output == null)
            output = input.substring(0, input.lastIndexOf('.'));

        main(input, output, repositories, keepTmpDir);
    }

    public static void main(String input, String output, List<ModuleRepository> repositories, boolean keepTmpDir) throws IOException, SyntaxErrors, SyntaxError {
        repositories.add(ResourceRepository.INSTANCE);
        //FrontEnd
        State.get().setImportResolver(new ImportResolver(repositories));
        State.get().setTempDir(Files.createTempDir());
        try {
            Module module = Parser.parse(Paths.get(input), Style.DEFAULT_STYLE);

            //Lowering Passes
            for (Module m : module.findImportedModulesReflexiveTransitive()) {
                FForEachLowering.lower(m);
                FLambdaIfLowering.lower(m);
            }

            //Reachability analysis
            Reachability reachability = Reachability.analyse(Collections.singleton(module.findMain()));

            //remove unreachable fields & functions from reachable classes
            for (Map.Entry<FClass, Reachability.ReachableClass> entry : reachability.getReachableClasses().entrySet())
                entry.getKey().removeUnreachable(entry.getValue());

            //bake
            for (Map.Entry<FClass, Reachability.ReachableClass> fClass : reachability.getReachableClasses().entrySet()) {
                if (fClass.getKey() instanceof FInstantiatedClass)
                    ((FInstantiatedClass) fClass.getKey()).bake();
                for (FInstantiatedFunction instantiation : fClass.getValue().reachableFunctions.values()) {
                    if (instantiation != null)
                        instantiation.bake();
                }
            }

            //remove bases of instantiated functions
            for (Map.Entry<FClass, Reachability.ReachableClass> fClass : reachability.getReachableClasses().entrySet()) {
                for (Map.Entry<FFunction, Collection<FInstantiatedFunction>> entry : fClass.getValue().reachableFunctions.asMap().entrySet()) {
                    Collection<FInstantiatedFunction> instantiations = entry.getValue();
                    if (instantiations.size() > 1 || instantiations.iterator().next() != null) {
                        FFunction baseFunction = entry.getKey();
                        fClass.getKey().getFunctions(false).get(baseFunction.getIdentifier()).remove(baseFunction.getSignature());
                    }
                }
            }

            //Backend
            LLVMBackend.runBackend(module, reachability, output, outputType);
        } finally {
            if (!keepTmpDir)
                FileUtils.deleteDir(State.get().getTempDir());
            State.get().setTempDir(null);
        }
    }
}

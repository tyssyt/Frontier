package tys.frontier.main;

import tys.frontier.State;
import tys.frontier.backend.llvm.LLVMBackend;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FInstantiatedFunction;
import tys.frontier.code.module.Module;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.parser.Parser;
import tys.frontier.parser.dependencies.ImportResolver;
import tys.frontier.parser.syntaxErrors.CyclicModuleDependency;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.parser.syntaxErrors.UnresolvableImport;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.passes.lowering.FForEachLowering;
import tys.frontier.passes.lowering.OperatorAssignmentLowering;
import tys.frontier.style.Style;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

public class Main {

    private static final LLVMBackend.OutputFileType outputType =
            LLVMBackend.OutputFileType.EXECUTABLE;

    public static void main(String[] args) throws UnresolvableImport, CyclicModuleDependency, SyntaxErrors, IOException {
            String input = args[0];
            String output = args.length >= 2 ? args[1] : input.substring(0, input.lastIndexOf('.'));
            main(input, output);
    }

    public static void main(String input, String output) throws IOException, SyntaxErrors, CyclicModuleDependency, UnresolvableImport {
        //FrontEnd
        State.get().setImportResolver(new ImportResolver());
        Module module = Parser.parse(Paths.get(input), Style.DEFAULT_STYLE);

        //Lowering Passes
        for (Module m : module.findImportedModulesReflexiveTransitive()) {
            FForEachLowering.lower(m);
            OperatorAssignmentLowering.lower(m);
        }

        //Reachability analysis
        @SuppressWarnings("OptionalGetWithoutIsPresent")
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
                    fClass.getKey().getFunctions().get(baseFunction.getIdentifier()).remove(baseFunction);
                }
            }
        }

        //Backend
        LLVMBackend.runBackend(module, reachability, output, outputType);
    }
}

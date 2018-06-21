package tys.frontier.parser.semanticAnalysis;

import com.google.common.graph.Graphs;
import tys.frontier.code.FClass;
import tys.frontier.code.module.ClassHierachy;
import tys.frontier.parser.syntaxErrors.CyclicClassHierachy;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;

import java.util.ArrayList;
import java.util.List;

public class CyclicClassHierachyCheck {

    private CyclicClassHierachyCheck() {}

    public static void check(ClassHierachy hierachy) throws SyntaxErrors {
        List<SyntaxError> errors = new ArrayList<>();

        //cycles
        if (Graphs.hasCycle(hierachy)) {
            //this is rather costly and should be a return result form the above call, but we won't finish compiling anyway
            for (FClass fClass : hierachy.nodes())
                if (Graphs.reachableNodes(hierachy, fClass).contains(fClass))
                    errors.add(new CyclicClassHierachy(fClass));
        }

        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }
}

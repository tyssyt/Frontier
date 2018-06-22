package tys.frontier.parser.semanticAnalysis;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.module.ClassHierachy;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CheckNamespaces {

    private CheckNamespaces() {}

    public static void check(ClassHierachy hierachy) throws SyntaxErrors {
        List<SyntaxError> errors = new ArrayList<>();

        for (FClass fClass : hierachy.nodes()) {
            if (!fClass.getSubClasses().isEmpty())
                continue;

            Map<FVariableIdentifier, FField> seenFields = new HashMap<>();
            Multimap<FFunctionIdentifier, FFunction> seenFunctions = HashMultimap.create();
            for (FClass c : fClass.getAllInheritedTypes()) {
                //fields
                for (FField field : c.getFields().values()) {
                    FField old = seenFields.put(field.getIdentifier(), field);
                    if (old != null)
                        errors.add(new IdentifierCollision(field, old));
                }
                //functions TODO when we introduce function calls with named params this needs major rework
                for (FFunction function : c.getFunctions().values()) {
                    nextOld: for (FFunction oldFunction : seenFunctions.get(function.getIdentifier())) {
                        int max = Math.max(
                                function.getSignature().getParamTypes().size(),
                                oldFunction.getSignature().getParamTypes().size()
                        );
                        List<FClass> paramTypes = function.getSignature().getAllParamTypes();
                        List<FClass> oldParamTypes = oldFunction.getSignature().getAllParamTypes();
                        if (paramTypes.size() < max || oldParamTypes.size() < max)
                            continue nextOld;
                        for (int i = 0; i < max; i++) {
                            if (paramTypes.get(i) != oldParamTypes.get(i))
                                continue nextOld;
                        }
                        errors.add(new IdentifierCollision(function, oldFunction));
                    }
                    seenFunctions.put(function.getIdentifier(), function);
                }
            }


        }

        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }
}

package tys.frontier.parser.syntaxTree;

import tys.frontier.code.FClass;
import tys.frontier.code.FConstructor;
import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.module.ClassHierachy;
import tys.frontier.parser.syntaxErrors.AbstractClassConstructorCall;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class RemoveAbstractConstructors {

    private RemoveAbstractConstructors() {}

    public static void remove(ClassHierachy hierachy) throws SyntaxErrors {
        List<SyntaxError> errors = new ArrayList<>();

        for (FClass fClass : hierachy.nodes()) {
            if (fClass.isAbstract()) {
                Collection<FFunction> constructors = fClass.getFunctions(FConstructor.IDENTIFIER);
                for (FFunction constructor : constructors) {
                    for (FFunctionCall functionCall : constructor.getCalledBy()) {
                        errors.add(new AbstractClassConstructorCall(functionCall));
                    }
                }
                constructors.clear();
            }
        }

        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }

}

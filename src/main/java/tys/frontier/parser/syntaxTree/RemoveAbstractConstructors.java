package tys.frontier.parser.syntaxTree;

import tys.frontier.code.FClass;
import tys.frontier.code.FType;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.module.ClassHierachy;
import tys.frontier.parser.syntaxErrors.AbstractClassConstructorCall;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;

import java.util.ArrayList;
import java.util.List;

public class RemoveAbstractConstructors {

    private RemoveAbstractConstructors() {}

    public static void remove(ClassHierachy hierachy) throws SyntaxErrors {
        List<SyntaxError> errors = new ArrayList<>();

        for (FType fType : hierachy.nodes()) {
            if (fType instanceof FClass) {
                FClass fClass = ((FClass) fType);
                if (fClass.isAbstract()) {
                    for (FFunctionCall functionCall : fClass.getConstructor().getCalledBy())
                        errors.add(new AbstractClassConstructorCall(functionCall));
                    fClass.removeConstructor();
                }
            }
        }

        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }

}

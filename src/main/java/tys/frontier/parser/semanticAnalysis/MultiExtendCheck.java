package tys.frontier.parser.semanticAnalysis;

import tys.frontier.code.FClass;
import tys.frontier.code.module.ClassHierachy;
import tys.frontier.parser.syntaxErrors.MultiExtend;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;

import java.util.*;

public class MultiExtendCheck {

    private MultiExtendCheck(){}

    public static void check(ClassHierachy hierachy) throws SyntaxErrors {
        List<SyntaxError> errors = new ArrayList<>();

        for (FClass fClass : hierachy.nodes()) {
            if (!fClass.getSubClasses().isEmpty())
                continue;
            Set<FClass> seenTypes = new HashSet<>();
            Queue<FClass> todo = new ArrayDeque<>();
            todo.add(fClass);
            while (!todo.isEmpty()) {
                FClass cur = todo.remove();
                for (FClass superClass : cur.getSuperClasses()) {
                    if (seenTypes.add(superClass))
                        todo.add(superClass);
                    else if (!superClass.getFields().isEmpty())
                        errors.add(new MultiExtend(fClass, superClass));
                }
            }
        }

        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }
}

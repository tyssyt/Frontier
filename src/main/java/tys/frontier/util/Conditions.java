package tys.frontier.util;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.literal.FBoolLiteral;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.predefinedClasses.FOptional;

import java.util.*;

public class Conditions {

    private static Signature AND = BinaryOperator.AND.getFunction(FBool.INSTANCE.getNamespace()).getSignature();

    private Conditions() {}

    public static List<FExpression> splitOnAnd(FExpression cond) {
        assert cond.getType() == FBool.INSTANCE;
        List<FExpression> res = new ArrayList<>();
        Queue<FExpression> todo = new ArrayDeque<>();
        todo.add(cond);
        while (!todo.isEmpty()) {
            FExpression cur = todo.remove();
            if (cur instanceof FFunctionCall && ((FFunctionCall) cur).getFunction().getIdentifier().equals(BinaryOperator.AND.identifier))
                todo.addAll(((FFunctionCall) cur).getArguments(true));
            else
                res.add(cur);
        }
        return res;
    }

    public static boolean isOptionalExistAtom(FExpression atom) {
        assert atom.getType() == FBool.INSTANCE;
        return atom instanceof FImplicitCast && ((FImplicitCast) atom).getTypeCast().getBase() instanceof FOptional;
    }

    public static boolean isOptionalNotExistsAtom(FExpression atom) {
        assert atom.getType() == FBool.INSTANCE;
        return atom instanceof FFunctionCall
                && ((FFunctionCall) atom).getFunction().getIdentifier().equals(UnaryOperator.NOT.identifier)
                && ((FFunctionCall) atom).getFunction().getMemberOf() instanceof FOptional;
    }

    public static FExpression and(FExpression atom1, FExpression atom2) {
        assert atom1.getType() == FBool.INSTANCE && atom2.getType() == FBool.INSTANCE;
        return FFunctionCall.createTrusted(AND, Arrays.asList(atom1, atom2));
    }

    public static FExpression and (FExpression... atoms) {
        return and(Arrays.asList(atoms));
    }

    public static FExpression and(List<FExpression> atoms) {
        if (atoms.isEmpty())
            return new FLiteralExpression(FBoolLiteral.TRUE);

        Iterator<FExpression> it = atoms.iterator();
        FExpression res = it.next();
        while (it.hasNext())
            res = and(res, it.next());
        return res;
    }

}

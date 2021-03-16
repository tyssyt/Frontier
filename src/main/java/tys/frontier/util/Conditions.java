package tys.frontier.util;

import tys.frontier.code.expression.FBracketsExpression;
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

import static java.util.Arrays.asList;

public class Conditions {

    private static Signature AND = BinaryOperator.AND.getFunctionTrusted(FBool.INSTANCE, FBool.INSTANCE);

    private Conditions() {}

    public static FExpression removeBrackets(FExpression cond) {
        while (cond instanceof FBracketsExpression)
            cond = ((FBracketsExpression) cond).getInner();
        return cond;
    }

    public static boolean isBooleanNot(FExpression cond) {
        assert cond.getType() == FBool.INSTANCE;
        if (cond instanceof FFunctionCall) {
            FFunctionCall functionCall = (FFunctionCall) cond;
            if (functionCall.getFunction().getIdentifier().equals(UnaryOperator.NOT.getIdentifier())
                    && functionCall.getFunction().getMemberOf().getType() == FBool.INSTANCE)
                return true;
        }
        return false;
    }


    public static boolean isBinaryBooleanOp(FExpression cond, BinaryOperator operator) {
        assert cond.getType() == FBool.INSTANCE;
        if (cond instanceof FFunctionCall) {
            FFunctionCall functionCall = (FFunctionCall) cond;
            if (functionCall.getFunction().getIdentifier().equals(operator.getIdentifier())
                    && functionCall.getArguments(false).get(0).getType() == FBool.INSTANCE
                    && functionCall.getArguments(false).get(1).getType() == FBool.INSTANCE
            )
                return true;
        }
        return false;
    }

    public static List<FExpression> splitOnOp(FExpression cond, BinaryOperator operator) {
        List<FExpression> res = new ArrayList<>();
        Queue<FExpression> todo = new ArrayDeque<>();
        todo.add(cond);
        while (!todo.isEmpty()) {
            FExpression cur = removeBrackets(todo.remove());
            assert cur.getType() == FBool.INSTANCE;
            if (isBinaryBooleanOp(cur, operator))
                todo.addAll(((FFunctionCall) cur).getArguments(false));
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
                && ((FFunctionCall) atom).getFunction().getMemberOf().getType() instanceof FOptional;
    }

    //TODO @PositionForGeneratedCode, but here it's not even generated and I might have a Position if I try
    public static FExpression and(FExpression atom1, FExpression atom2) {
        assert atom1.getType() == FBool.INSTANCE && atom2.getType() == FBool.INSTANCE;
        return FFunctionCall.createTrusted(null, AND, asList(atom1, atom2));
    }

    //TODO @PositionForGeneratedCode
    public static FExpression and(List<FExpression> atoms) {
        if (atoms.isEmpty())
            return new FLiteralExpression(null, FBoolLiteral.TRUE);

        Iterator<FExpression> it = atoms.iterator();
        FExpression res = it.next();
        while (it.hasNext())
            res = and(res, it.next());
        return res;
    }
}

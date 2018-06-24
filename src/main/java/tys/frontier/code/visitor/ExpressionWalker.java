package tys.frontier.code.visitor;

import tys.frontier.code.expression.*;

public interface ExpressionWalker<Expression> {

    default Expression visitArrayAccess(FArrayAccess arrayAccess) {
        arrayAccess.getObject().accept(this);
        arrayAccess.getIndex().accept(this);
        return null;
    }

    default Expression visitBrackets(FBracketsExpression brackets) {
        brackets.getInner().accept(this);
        return null;
    }

    default Expression visitFunctionCall(FFunctionCall functionCall) {
        if (functionCall.getObject() != null)
            functionCall.getObject().accept(this);
        for (FExpression param : functionCall.getArguments())
            param.accept(this);
        return null;
    }

    default Expression visitFieldAccess(FFieldAccess fieldAccess) {
        if (fieldAccess.getObject() != null)
                fieldAccess.getObject().accept(this);
        return null;
    }

    default Expression visitImplicitCast(FImplicitCast implicitCast) {
        return implicitCast.getCastedExpression().accept(this);
    }

    default Expression visitExplicitCast(FExplicitCast explicitCast) {
        return explicitCast.getCastedExpression().accept(this);
    }

    default Expression visitLiteral(FLiteralExpression expression) {
        return null;
    }

    default Expression visitVariable(FLocalVariableExpression expression) {
        return null;
    }
}
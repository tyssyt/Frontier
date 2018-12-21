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
        for (FExpression param : functionCall.getArguments())
            param.accept(this);
        return null;
    }

    default Expression visitDynamicFunctionCall(DynamicFunctionCall functionCall) {
        functionCall.getFunction().accept(this);
        for (FExpression param : functionCall.getArguments()) {
            param.accept(this);
        }
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

    default Expression visitOptElse(FOptElse optElse) {
        optElse.getOptional().accept(this);
        optElse.getElse().accept(this);
        return null;
    }

    default Expression visitLiteral(FLiteralExpression expression) {
        return null;
    }

    default Expression visitVariable(FLocalVariableExpression expression) {
        return null;
    }

    default Expression visitClassExpr(FClassExpression expression) {
        return null;
    }

    default Expression visitFunctionAddress(FFunctionAddress expression) {
        return null;
    }
}
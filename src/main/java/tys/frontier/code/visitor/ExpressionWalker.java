package tys.frontier.code.visitor;

import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FImplicitCast;

public interface ExpressionWalker<Expression> {

    default Expression visitFunctionCall(FFunctionCall functionCall) {
        for (FExpression param : functionCall.getArguments(true))
            param.accept(this);
        return null;
    }

    default Expression visitDynamicFunctionCall(DynamicFunctionCall functionCall) {
        functionCall.getFunction().accept(this);
        for (FExpression param : functionCall.getArguments())
            param.accept(this);
        return null;
    }

    default Expression visitImplicitCast(FImplicitCast implicitCast) {
        return implicitCast.getCastedExpression().accept(this);
    }

    default Expression visitOptElse(FOptElse optElse) {
        optElse.getOptional().accept(this);
        optElse.getElse().accept(this);
        return null;
    }

    default Expression visitCache(FCacheExpression cache) {
        cache.getExpression().accept(this);
        return null;
    }

    default Expression visitArrayLiteral(FArrayLiteral expression) {
        for (FExpression element : expression.getElements())
            element.accept(this);
        return null;
    }

    default Expression visitLiteral(FLiteralExpression expression) {
        return null;
    }

    default Expression visitVariable(FVariableExpression expression) {
        return null;
    }

    default Expression visitNamespaceExpression(FNamespaceExpression expression) {
        return null;
    }

    default Expression visitFunctionAddress(FFunctionAddress expression) {
        return null;
    }

    default Expression visitPack(Pack pack) {
        for (FExpression expression : pack.getExpressions())
            expression.accept(this);
        return null;
    }

    default Expression visitUnpack(Unpack unpack) {
        unpack.getUnpackedExpression().accept(this);
        return null;
    }

    default Expression visitUnpackedElement(Unpack.UnpackedElement unpackedElement) {
        if (unpackedElement.isLast())
            unpackedElement.getUnpack().accept(this);
        return null;
    }
}
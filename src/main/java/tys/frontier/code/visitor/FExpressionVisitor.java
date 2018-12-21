package tys.frontier.code.visitor;

import tys.frontier.code.expression.*;

import java.util.List;

public interface FExpressionVisitor extends ExpressionVisitor<FExpression> {

    @Override
    default FExpression exitArrayAccess(FArrayAccess arrayAccess, FExpression array, FExpression index) {
        return arrayAccess;
    }

    @Override
    default FExpression exitBrackets(FBracketsExpression brackets, FExpression inner) {
        return brackets;
    }

    @Override
    default FExpression exitFunctionCall(FFunctionCall functionCall,  List<FExpression> params) {
        return functionCall;
    }

    @Override
    default FExpression exitDynamicFunctionCall(DynamicFunctionCall functionCall, FExpression function, List<FExpression> params) {
        return functionCall;
    }

    @Override
    default FExpression exitFieldAccess(FFieldAccess fieldAccess, FExpression object) {
        return fieldAccess;
    }

    @Override
    default FExpression exitImplicitCast(FImplicitCast implicitCast, FExpression castedExpression) {
        return implicitCast;
    }

    @Override
    default FExpression exitExplicitCast(FExplicitCast explicitCast, FExpression castedExpression) {
        return explicitCast;
    }

    @Override
    default FExpression visitLiteral(FLiteralExpression expression) {
        return expression;
    }

    @Override
    default FExpression visitVariable(FLocalVariableExpression expression) {
        return expression;
    }

    @Override
    default FExpression visitClassExpr(FClassExpression expression) {
        return expression;
    }

    @Override
    default FExpression visitFunctionAddress(FFunctionAddress address) {
        return address;
    }
}

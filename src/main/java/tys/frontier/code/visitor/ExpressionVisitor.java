package tys.frontier.code.visitor;

import tys.frontier.code.expression.*;

import java.util.List;

public interface ExpressionVisitor<Expression>  {

    //Top down
    default void enterArrayAccess(FArrayAccess arrayAccess) {}
    default void enterBrackets(FBracketsExpression brackets) {}
    default void enterFunctionCall(FFunctionCall functionCall) {}
    default void enterFieldAccess(FFieldAccess fieldAccess) {}
    default void enterImplicitCast(FImplicitCast implicitCast) {}
    default void enterExplicitCast(FExplicitCast explicitCast) {}
    default void enterOptElse(FOptElse optElse) {}

    //Bottom Up
    default Expression exitArrayAccess(FArrayAccess arrayAccess, Expression array, Expression index) {
        return null;
    }
    default Expression exitBrackets(FBracketsExpression brackets, Expression inner) {
        return null;
    }
    default Expression exitFunctionCall(FFunctionCall functionCall, Expression object, List<Expression> params) {
        return null;
    }
    default Expression exitFieldAccess(FFieldAccess fieldAccess, Expression object) {
        return null;
    }
    default Expression exitImplicitCast(FImplicitCast implicitCast, Expression castedExpression) {return null;}
    default Expression exitExplicitCast(FExplicitCast explicitCast, Expression castedExpression) {return null;}
    default Expression exitOptElse(FOptElse optElse, Expression optional, Expression elze) {return null;}

    //Leaves
    default Expression visitLiteral(FLiteralExpression expression) {
        return null;
    }
    default Expression visitVariable(FLocalVariableExpression expression) {
        return null;
    }

}

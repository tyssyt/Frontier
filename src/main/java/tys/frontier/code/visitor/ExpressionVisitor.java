package tys.frontier.code.visitor;

import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FImplicitCast;

import java.util.List;

public interface ExpressionVisitor<Expression>  {

    //Top down
    default void enterBrackets(FBracketsExpression brackets) {}
    default boolean enterFunctionCall(FFunctionCall functionCall) {return true;} //boolean visitDefaults
    default void enterDynamicFunctionCall(DynamicFunctionCall functionCall) {}
    default void enterImplicitCast(FImplicitCast implicitCast) {}
    default void enterOptElse(FOptElse optElse) {}
    default void enterCache(FCacheExpression cache) {}

    //Bottom Up
    default Expression exitBrackets(FBracketsExpression brackets, Expression inner) {
        return null;
    }
    default Expression exitFunctionCall(FFunctionCall functionCall, List<Expression> params) {
        return null;
    }
    default Expression exitDynamicFunctionCall(DynamicFunctionCall functionCall, Expression function, List<Expression> params) {return null;}
    default Expression exitImplicitCast(FImplicitCast implicitCast, Expression castedExpression) {return null;}
    default Expression exitOptElse(FOptElse optElse, Expression optional, Expression elze) {return null;}
    default Expression exitCache(FCacheExpression cache, Expression expression) {return null;}

    //Leaves
    default Expression visitLiteral(FLiteralExpression expression) {
        return null;
    }
    default Expression visitVariable(FVariableExpression expression) {
        return null;
    }
    default Expression visitNamespaceExpression(FNamespaceExpression expression) {
        return null;
    }
    default Expression visitFunctionAddress(FFunctionAddress address) {
        return null;
    }

}

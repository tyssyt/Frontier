package tys.frontier.code.visitor;

import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FImplicitCast;

import java.util.List;
import java.util.Optional;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public interface ExpressionVisitor<Expression>  {

    //Top down
    default boolean enterFunctionCall(FFunctionCall functionCall) {return true;} //boolean visitDefaults
    default void enterDynamicFunctionCall(DynamicFunctionCall functionCall) {}
    default void enterImplicitCast(FImplicitCast implicitCast) {}
    default void enterOptElse(FOptElse optElse) {}
    default void enterCache(FCacheExpression cache) {}
    default void enterArrayLiteral(FArrayLiteral arrayLiteral) {}
    default void enterPack(Pack pack) {}
    default void enterUnpack(Unpack unpack) {}
    default boolean enterUnpackedElement(Unpack.UnpackedElement unpackedElement) {return unpackedElement.isLast();}

    //Bottom Up
    default Expression exitFunctionCall(FFunctionCall functionCall, List<Expression> args) {return null;}
    default Expression exitDynamicFunctionCall(DynamicFunctionCall functionCall, Expression function, List<Expression> args) {return null;}
    default Expression exitImplicitCast(FImplicitCast implicitCast, Expression castedExpression) {return null;}
    default Expression exitOptElse(FOptElse optElse, Expression optional, Expression elze) {return null;}
    default Expression exitCache(FCacheExpression cache, Expression expression) {return null;}
    default Expression exitArrayLiteral(FArrayLiteral arrayLiteral, List<Expression> elements) {return null;}
    default Expression exitPack(Pack pack, List<Expression> expressions) {return null;}
    default Expression exitUnpack(Unpack unpack, Expression unpackedExpression) {return null;}
    default Expression exitUnpackedElement(Unpack.UnpackedElement unpackedElement, Optional<Expression> unpack) {return null;}

    //Leaves
    default Expression visitLiteral(FLiteralExpression expression) {return null;}
    default Expression visitVariable(FVariableExpression expression) {return null;}
    default Expression visitNamespaceExpression(FNamespaceExpression expression) {return null;}
    default Expression visitFunctionAddress(FFunctionAddress address) {return null;}

}

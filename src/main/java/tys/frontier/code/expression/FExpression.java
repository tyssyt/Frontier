package tys.frontier.code.expression;

import tys.frontier.code.Typed;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.util.StringBuilderToString;

public interface FExpression extends Typed, StringBuilderToString {

    <E> E accept(ExpressionVisitor<E> visitor);

}

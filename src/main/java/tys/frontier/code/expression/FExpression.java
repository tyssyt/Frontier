package tys.frontier.code.expression;

import tys.frontier.code.Typed;
import tys.frontier.code.visitor.ExpressionVisitor;

public interface FExpression extends Typed {

    <E> E accept(ExpressionVisitor<E> visitor);

}

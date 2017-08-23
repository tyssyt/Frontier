package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.visitor.ExpressionVisitor;

public class FBracketsExpression implements FExpression {

    private FExpression inner;

    public FBracketsExpression(FExpression inner) {
        this.inner = inner;
    }

    public FExpression getInner() {
        return inner;
    }

    @Override
    public FClass getType() {
        return inner.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.enterBrackets(this);
    }
}

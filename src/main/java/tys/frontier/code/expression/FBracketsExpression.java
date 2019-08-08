package tys.frontier.code.expression;

import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

public class FBracketsExpression implements FExpression {

    private FExpression inner;

    public FBracketsExpression(FExpression inner) {
        this.inner = inner;
    }

    public FExpression getInner() {
        return inner;
    }

    @Override
    public FType getType() {
        return inner.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterBrackets(this);
        return visitor.exitBrackets(this, inner.accept(visitor));
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitBrackets(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append('(');
        return inner.toString(sb).append(')');
    }
    @Override
    public String toString() {
        return tS();
    }
}

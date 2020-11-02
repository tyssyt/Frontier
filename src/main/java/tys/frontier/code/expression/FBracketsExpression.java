package tys.frontier.code.expression;

import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;

public class FBracketsExpression extends FExpression {

    private FExpression inner;

    public FBracketsExpression(Position position, FExpression inner) {
        super(position);
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
}

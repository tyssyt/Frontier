package tys.frontier.code.statement;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;

public class FExpressionStatement implements FStatement {

    private FExpression expression;

    public FExpressionStatement(FExpression expression) {
        this.expression = expression;
    }

    public FExpression getExpression() {
        return expression;
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterExpressionStatement(this);
        return visitor.exitExpressionStatement(this, expression.accept(visitor));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitExpressionStatement(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return expression.toString(sb).append(';');
    }
    @Override
    public String toString() {
        return tS();
    }
}

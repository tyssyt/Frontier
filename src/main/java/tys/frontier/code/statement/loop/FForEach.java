package tys.frontier.code.statement.loop;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;

public class FForEach extends FLoop {

    private FLocalVariable iterator;
    private FExpression container;

    private FForEach(int nestedDepth, FLoopIdentifier identifier, FLocalVariable iterator, FExpression container, FBlock body) {
        super(nestedDepth, identifier, body);
        this.iterator = iterator;
        this.container = container;
    }

    public static FForEach create(int nestedDepth, FLoopIdentifier identifier, FLocalVariable iterator, FExpression container, FBlock body) {
        return new FForEach(nestedDepth, identifier, iterator, container, body);
    }

    public FLocalVariable getIterator() {
        return iterator;
    }

    public FExpression getContainer() {
        return container;
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterForEach(this);
        return visitor.exitForEach(this, container.accept(visitor), getBody().accept(visitor));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitForEach(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("for (").append(iterator).append(" : ");
        container.toString(sb).append(") ");
        return getBody().toString(sb);
    }
    @Override
    public String toString() {
        return tS();
    }
}

package tys.frontier.code.statement.loop;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.util.Utils;

import java.util.List;
import java.util.Optional;

public class FForEach extends FLoop {

    private List<FLocalVariable> iterators;
    private FLocalVariable counter;
    private FExpression container;

    private FForEach(Position position, int nestedDepth, FLoopIdentifier identifier, List<FLocalVariable> iterators, FLocalVariable counter, FExpression container, FBlock body) {
        super(position, nestedDepth, identifier, body);
        this.iterators = iterators;
        this.counter = counter;
        this.container = container;
    }

    public static FForEach create(Position position, int nestedDepth, FLoopIdentifier identifier, List<FLocalVariable> iterators, FLocalVariable counter, FExpression container, FBlock body) {
        return new FForEach(position, nestedDepth, identifier, iterators, counter, container, body);
    }

    public List<FLocalVariable> getIterators() {
        return iterators;
    }

    public Optional<FLocalVariable> getCounter() {
        return Optional.ofNullable(counter);
    }

    public FExpression getContainer() {
        return container;
    }

    public ForImpl getForImpl() {
        return container.getType().getForImpl();
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
        sb.append("for ");
        Utils.joinIdentifiers(sb, iterators, ", ").append(" : ");
        container.toString(sb).append(") ");
        return getBody().toString(sb);
    }
}

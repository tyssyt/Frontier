package tys.frontier.code.statement;

import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;

import java.util.Optional;

public class FEmptyStatement implements FStatement {

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return Optional.empty();
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.visitEmpty(this);
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitEmpty(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(';');
    }

    @Override
    public String toString() {
        return ";";
    }
}

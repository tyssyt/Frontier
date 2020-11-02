package tys.frontier.code.statement;

import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.util.StringBuilderToString;

import java.util.Optional;

public abstract class FStatement implements StringBuilderToString {

    protected Position position;

    protected FStatement(Position position) {
        this.position = position;
    }

    public abstract Optional<ControlFlowIDontKnow> redirectsControlFlow();
    public abstract <S, E> S accept(StatementVisitor<S, E> visitor);
    public abstract <S, E> S accept(StatementWalker<S, E> walker);

    public Position getPosition() {
        return position;
    }

    @Override
    public String toString() {
        return tS();
    }
}

package tys.frontier.code.statement.loop;

import tys.frontier.code.statement.ControlFlowIDontKnow;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;

import java.util.Optional;

public class FBreak implements FStatement {

    private FLoopIdentifier loop;

    public FBreak(FLoopIdentifier loop) {
        this.loop = loop;
    }

    public FLoopIdentifier getLoop() {
        return loop;
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return Optional.of(loop.getLoop());
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.visitBreak(this);
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitBreak(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append("break;");
    }
    @Override
    public String toString() {
        return "break;";
    }
}

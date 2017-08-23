package tys.frontier.code.statement.loop;

import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.StatementVisitor;

public class FContinue implements FStatement {

    private FLoopIdentifier loop;

    public FContinue(FLoopIdentifier loop) {
        this.loop = loop;
    }

    public FLoopIdentifier getLoop() {
        return loop;
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.visitContinue(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append("continue;");
    }
    @Override
    public String toString() {
        return "continue;";
    }
}

package tys.frontier.code.statement;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.visitor.StatementVisitor;

public class FBlock implements FStatement {

    private ImmutableList<FStatement> statements;

    public FBlock(ImmutableList<FStatement> statements) {
        this.statements = statements;
    }

    public ImmutableList<FStatement> getStatements() {
        return statements;
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.enterBlock(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("{\n");
        for (FStatement statement : statements)
            statement.toString(sb).append('\n');
        return sb.append('}');
    }
    @Override
    public String toString() {
        return tS();
    }
}

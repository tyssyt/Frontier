package tys.frontier.code.statement;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.visitor.StatementVisitor;

import java.util.ArrayList;
import java.util.List;

public class FBlock implements FStatement {

    private ImmutableList<FStatement> statements;

    public FBlock(ImmutableList<FStatement> statements) {
        this.statements = statements;
    }

    public ImmutableList<FStatement> getStatements() {
        return statements;
    }

    public List<FStatement> flattenRecursive() {
        List<FStatement> res = new ArrayList<>();
        for(FStatement statement : statements) {
            if (statement instanceof FBlock)
                res.addAll(((FBlock) statement).flattenRecursive());
            else
                res.add(statement);
        }
        return res;
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterBlock(this);
        List<S> statements = new ArrayList<>(this.statements.size());
        for (FStatement s : this.statements)
            statements.add(visitor.visit(s));
        return visitor.exitBlock(this, statements);
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

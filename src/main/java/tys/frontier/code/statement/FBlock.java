package tys.frontier.code.statement;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class FBlock implements FStatement {

    private ImmutableList<FStatement> statements;

    public FBlock(ImmutableList<FStatement> statements) {
        assert statements.size() > 1; //size 0 is EmptyStatement, size 1 is just the statement
        assert statements.stream().limit(statements.size()-1).noneMatch( s -> s.redirectsControlFlow().isPresent());
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
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return Iterables.getLast(statements).redirectsControlFlow();
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterBlock(this);
        List<S> statements = new ArrayList<>(this.statements.size());
        for (FStatement s : this.statements)
            statements.add(s.accept(visitor));
        return visitor.exitBlock(this, statements);
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitBlock(this);
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

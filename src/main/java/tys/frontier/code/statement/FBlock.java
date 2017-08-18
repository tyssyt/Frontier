package tys.frontier.code.statement;

import com.google.common.collect.ImmutableList;

public class FBlock implements FStatement {

    private ImmutableList<FStatement> statements;

    public FBlock(ImmutableList<FStatement> statements) {
        this.statements = statements;
    }

    public ImmutableList<FStatement> getStatements() {
        return statements;
    }
}

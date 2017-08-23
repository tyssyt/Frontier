package tys.frontier.code.statement;

import tys.frontier.code.visitor.StatementVisitor;

public class FEmptyStatement implements FStatement {
    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.visitEmpty(this);
    }
}

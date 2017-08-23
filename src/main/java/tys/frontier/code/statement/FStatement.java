package tys.frontier.code.statement;

import tys.frontier.code.visitor.StatementVisitor;

public interface FStatement {

    <S, E> S accept(StatementVisitor<S, E> visitor);

}

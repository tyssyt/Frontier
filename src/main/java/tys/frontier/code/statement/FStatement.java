package tys.frontier.code.statement;

import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.util.StringBuilderToString;

public interface FStatement extends StringBuilderToString {

    <S, E> S accept(StatementVisitor<S, E> visitor);

}

package tys.frontier.code.statement;

import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.util.StringBuilderToString;

import java.util.Optional;

public interface FStatement extends StringBuilderToString {

    Optional<ControlFlowIDontKnow> redirectsControlFlow();

    <S, E> S accept(StatementVisitor<S, E> visitor);
    <S, E> S accept(StatementWalker<S, E> walker);

}

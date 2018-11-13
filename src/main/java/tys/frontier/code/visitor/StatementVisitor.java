package tys.frontier.code.visitor;

import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;

import java.util.List;
import java.util.Optional;

public interface StatementVisitor<Statement, Expression> extends ExpressionVisitor<Expression> {

    //Top Down
    default void enterBlock(FBlock block) {}
    default void enterExpressionStatement(FExpressionStatement statement) {}
    default void enterIf(FIf fIf) {}
    default void enterReturn(FReturn fReturn) {}
    default void enterVarDeclaration(FVarDeclaration declaration) {}
    default void enterVarAssignment(FVarAssignment assignment) {}
    default void enterWhile(FWhile fWhile) {}
    default void enterFor(FFor fFor) {}
    default void enterForEach(FForEach forEach) {}

    //Bottom Up
    default Statement exitBlock(FBlock block, List<Statement> statements) {
        return null;
    }
    default Statement exitExpressionStatement(FExpressionStatement statement, Expression expression) {
        return null;
    }
    default Statement exitIf(FIf fIf, Expression cond, Statement then, Optional<Statement> elze) {
        return null;
    }
    default Statement exitReturn(FReturn fReturn, Optional<Expression> value) {
        return null;
    }
    default Statement exitVarDeclaration(FVarDeclaration declaration, Optional<Expression> value) {
        return null;
    }
    default Statement exitVarAssignment(FVarAssignment assignment, Expression variable, Expression value) {
        return null;
    }
    default Statement exitWhile(FWhile fWhile, Expression cond, Statement body) {
        return null;
    }
    default Statement exitFor(FFor fFor, Optional<Statement> declaration, Optional<Expression> condition,
                      Optional<Expression> increment, Statement body) {
        return null;
    }
    default Statement exitForEach(FForEach forEach, Expression container, Statement body) {
        return null;
    }

    //Leaves
    default Statement visitBreak(FBreak fBreak) {
        return null;
    }
    default Statement visitContinue(FContinue fContinue) {
        return null;
    }
}

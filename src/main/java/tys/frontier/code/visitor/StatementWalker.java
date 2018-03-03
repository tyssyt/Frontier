package tys.frontier.code.visitor;

import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;

public interface StatementWalker<Statement, Expression> extends ExpressionWalker<Expression> {

    //Top Down
    default Statement visitBlock(FBlock block) {
        for (FStatement statement : block.getStatements())
            statement.accept(this);
        return null;
    }

    default Statement visitExpressionStatement(FExpressionStatement statement) {
        statement.getExpression().accept(this);
        return null;
    }

    default Statement visitIf(FIf fIf) {
        fIf.getCondition().accept(this);
        fIf.getThen().accept(this);
        fIf.getElse().ifPresent(statement -> statement.accept(this));
        return null;
    }

    default Statement visitReturn(FReturn fReturn) {
        fReturn.getExpression().ifPresent(expression -> expression.accept(this));
        return null;
    }

    default Statement visitVarDeclaration(FVarDeclaration declaration) {
        return declaration.getAssignment().map(assignment -> assignment.accept(this)).orElse(null);
    }

    default Statement visitVarAssignment(FVarAssignment assignment) {
        assignment.getVariableExpression().accept(this);
        assignment.getValue().accept(this);
        return null;
    }

    default Statement visitWhile(FWhile fWhile) {
        fWhile.getCondition().accept(this);
        return fWhile.getBody().accept(this);
    }

    default Statement visitFor(FFor fFor) {
        fFor.getDeclaration().ifPresent(declaration -> declaration.accept(this));
        fFor.getCondition().ifPresent(condition -> condition.accept(this));
        fFor.getIncrement().ifPresent(increment -> increment.accept(this));
        return fFor.getBody().accept(this);
    }

    default Statement visitForEach(FForEach forEach) {
        forEach.getContainer().accept(this);
        return forEach.getBody().accept(this);
    }

    default Statement visitEmpty(FEmptyStatement statement) {
        return null;
    }

    default Statement visitBreak(FBreak fBreak) {
        return null;
    }

    default Statement visitContinue(FContinue fContinue) {
        return null;
    }
}

package tys.frontier.code.visitor;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.FBreak;
import tys.frontier.code.statement.loop.FContinue;
import tys.frontier.code.statement.loop.FForEach;
import tys.frontier.code.statement.loop.FWhile;

public interface StatementWalker<Statement, Expression> extends ExpressionWalker<Expression> {

    //Top Down
    default Statement visitBlock(FBlock block) {
        for (FStatement statement : block)
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
        for (FExpression expression : fReturn.getExpressions())
            expression.accept(this);
        return null;
    }

    default Statement visitVarAssignment(FAssignment assignment) {
        for (FExpression lhsExpression : assignment.getLhsExpressions())
            lhsExpression.accept(this);
        for (FExpression value : assignment.getValues())
            value.accept(this);
        return null;
    }

    default Statement visitWhile(FWhile fWhile) {
        fWhile.getCondition().accept(this);
        return fWhile.getBody().accept(this);
    }

    default Statement visitForEach(FForEach forEach) {
        forEach.getContainer().accept(this);
        return forEach.getBody().accept(this);
    }

    default Statement visitBreak(FBreak fBreak) {
        return null;
    }

    default Statement visitContinue(FContinue fContinue) {
        return null;
    }
}

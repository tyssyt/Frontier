package tys.frontier.code.visitor;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;

import java.util.List;
import java.util.Optional;

public interface FStatementVisitor extends FExpressionVisitor, StatementVisitor<FStatement, FExpression> {
    @Override
    default FStatement exitBlock(FBlock block, List<FStatement> fStatements) {
        return block;
    }

    @Override
    default FStatement exitExpressionStatement(FExpressionStatement statement, FExpression fExpression) {
        return statement;
    }

    @Override
    default FStatement exitIf(FIf fIf, FExpression cond, FStatement then, Optional<FStatement> elze) {
        return fIf;
    }

    @Override
    default FStatement exitReturn(FReturn fReturn, Optional<FExpression> value) {
        return fReturn;
    }

    @Override
    default FStatement exitVarDeclaration(FVarDeclaration declaration, Optional<FExpression> value) {
        return declaration;
    }

    @Override
    default FStatement exitVarAssignment(FVarAssignment assignment, FExpression variable, FExpression value) {
        return assignment;
    }

    @Override
    default FStatement exitWhile(FWhile fWhile, FExpression cond, FStatement body) {
        return fWhile;
    }

    @Override
    default FStatement exitFor(FFor fFor, Optional<FStatement> declaration, Optional<FExpression> condition, Optional<FExpression> increment, FStatement body) {
        return fFor;
    }

    @Override
    default FStatement exitForEach(FForEach forEach, FExpression container, FStatement body) {
        return forEach;
    }

    @Override
    default FStatement visitBreak(FBreak fBreak) {
        return fBreak;
    }

    @Override
    default FStatement visitContinue(FContinue fContinue) {
        return fContinue;
    }
}

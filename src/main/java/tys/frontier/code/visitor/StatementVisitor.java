package tys.frontier.code.visitor;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public interface StatementVisitor<Statement, Expression> {

    //
    Expression enterExpression(FExpression expression);

    //top-down pass
    Statement enterStatement(FStatement statement);

    Statement enterBlock(FBlock block);

    Statement enterExpressionStatement(FExpressionStatement statement);

    Statement enterIf(FIf fIf);

    Statement enterReturn(FReturn fReturn);

    Statement enterVarDeclaration(FVarDeclaration declaration);

    Statement enterVarAssignment(FVarAssignment assignment);

    Statement enterWhile(FWhile fWhile);

    Statement enterFor(FFor fFor);

    Statement enterForEach(FForEach forEach);

    //bottom-up pass
    Statement exitBlock(FBlock block, List<Statement> statements);

    Statement exitExpressionStatement(FExpressionStatement statement, Expression expression);

    Statement exitIf(FIf fIf, Expression cond, Statement then, Optional<Statement> elze);

    Statement exitReturn(FReturn fReturn, Optional<Expression> value);

    Statement exitVarDeclaration(FVarDeclaration declaration, Optional<Statement> value);

    Statement exitVarAssignment(FVarAssignment assignment, Optional<Expression> object, Expression value);

    Statement exitWhile(FWhile fWhile, Expression cond, Statement body);

    Statement exitFor(FFor fFor, Optional<Statement> declaration, Optional<Expression> condition,
                      Optional<Expression> increment, Statement body);

    Statement exitForEach(FForEach forEach, Expression container, Statement body);

    //leaves
    Statement visitEmpty(FEmptyStatement statement);

    Statement visitBreak(FBreak fBreak);

    Statement visitContinue(FContinue fContinue);

    abstract class Default<Statement, Expression> implements StatementVisitor<Statement, Expression> {
        private ExpressionVisitor<Expression> exprVis;

        public Default(ExpressionVisitor<Expression> expressionVisitor) {
            this.exprVis = expressionVisitor;
        }

        Statement getDefault() {
            return null;
        }

        //
        @Override
        public Expression enterExpression(FExpression expression) {
            return exprVis.enterExpression(expression);
        }

        //top-down
        @Override
        public Statement enterStatement(FStatement statement) {
            return statement.accept(this);
        }

        @Override
        public Statement enterBlock(FBlock block) {
            List<Statement> statements = new ArrayList<>(block.getStatements().size());
            for (FStatement s : block.getStatements())
                statements.add(enterStatement(s));
            return exitBlock(block, statements);
        }

        @Override
        public Statement enterExpressionStatement(FExpressionStatement statement) {
            return exitExpressionStatement(statement, enterExpression(statement.getExpression()));
        }

        @Override
        public Statement enterIf(FIf fIf) {
            Optional<Statement> elze = fIf.getElse().map(this::enterStatement);
            return exitIf(fIf, enterExpression(fIf.getCondition()), enterStatement(fIf.getThen()), elze);
        }

        @Override
        public Statement enterReturn(FReturn fReturn) {
            Optional<Expression> value = fReturn.getExpression().map(this::enterExpression);
            return exitReturn(fReturn, value);
        }

        @Override
        public Statement enterVarDeclaration(FVarDeclaration declaration) {
            Optional<Statement> value = declaration.getAssignment().map(this::enterStatement);
            return exitVarDeclaration(declaration, value);
        }

        @Override
        public Statement enterVarAssignment(FVarAssignment assignment) {
            Optional<Expression> object;
            if (assignment.getVariableExpression() instanceof FFieldAccess)
                object = Optional.of(enterExpression(((FFieldAccess) assignment.getVariableExpression()).getObject()));
            else
                object = Optional.empty();
            return exitVarAssignment(assignment, object, enterExpression(assignment.getValue()));
        }

        @Override
        public Statement enterWhile(FWhile fWhile) {
            return exitWhile(fWhile, enterExpression(fWhile.getCondition()), enterStatement(fWhile.getBody()));
        }

        @Override
        public Statement enterFor(FFor fFor) {
            Optional<Statement> decl = fFor.getDeclaration().map(this::enterStatement);
            Optional<Expression> cond = fFor.getCondition().map(this::enterExpression);
            Optional<Expression> inc = fFor.getIncrement().map(this::enterExpression);
            return exitFor(fFor, decl, cond, inc, enterStatement(fFor.getBody()));
        }

        @Override
        public Statement enterForEach(FForEach forEach) {
            return exitForEach(forEach, enterExpression(forEach.getContainer()), enterStatement(forEach.getBody()));
        }

        //bottom up
        @Override
        public Statement exitBlock(FBlock block, List<Statement> statements) {
            return getDefault();
        }

        @Override
        public Statement exitExpressionStatement(FExpressionStatement statement, Expression expression) {
            return getDefault();
        }

        @Override
        public Statement exitIf(FIf fIf, Expression cond, Statement then, Optional<Statement> elze) {
            return getDefault();
        }

        @Override
        public Statement exitReturn(FReturn fReturn, Optional<Expression> value) {
            return getDefault();
        }

        @Override
        public Statement exitVarDeclaration(FVarDeclaration declaration, Optional<Statement> value) {
            return getDefault();
        }

        @Override
        public Statement exitVarAssignment(FVarAssignment assignment, Optional<Expression> object, Expression value) {
            return getDefault();
        }

        @Override
        public Statement exitWhile(FWhile fWhile, Expression cond, Statement body) {
            return getDefault();
        }

        @Override
        public Statement exitFor(FFor fFor, Optional<Statement> declaration, Optional<Expression> condition, Optional<Expression> increment, Statement body) {
            return getDefault();
        }

        @Override
        public Statement exitForEach(FForEach forEach, Expression container, Statement body) {
            return getDefault();
        }

        //leafs
        @Override
        public Statement visitEmpty(FEmptyStatement statement) {
            return getDefault();
        }

        @Override
        public Statement visitBreak(FBreak fBreak) {
            return getDefault();
        }

        @Override
        public Statement visitContinue(FContinue fContinue) {
            return getDefault();
        }
    }
}

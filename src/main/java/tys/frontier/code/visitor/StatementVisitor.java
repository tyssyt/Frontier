package tys.frontier.code.visitor;

import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public interface StatementVisitor<Statement, Expression> {

    //top-down pass
    Statement enterStatement(FStatement statement);

    Statement enterBlock(FBlock block);

    Statement enterExpression(FExpressionStatement statement);

    Statement enterIf(FIf fIf);

    Statement enterReturn(FReturn fReturn);

    Statement enterVarDeclaration(FVarDeclaration declaration);

    Statement enterVarAssignment(FVarAssignment assignment);

    Statement enterWhile(FWhile fWhile);

    Statement enterFor(FFor fFor);

    Statement enterForEach(FForEach forEach);

    //bottom-up pass
    Statement exitBlock(FBlock block, List<Statement> statements);

    Statement exitExpression(FExpressionStatement statement, Expression expression);

    Statement exitIf(FIf fIf, Expression cond, Statement then, Optional<Statement> elze);

    Statement exitReturn(FReturn fReturn, Expression value);

    Statement exitVarDeclaration(FVarDeclaration declaration, Optional<Statement> value);

    Statement exitVarAssignment(FVarAssignment assignment, Expression value);

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
        public Statement enterExpression(FExpressionStatement statement) {
            return exitExpression(statement, exprVis.enterExpression(statement.getExpression()));
        }

        @Override
        public Statement enterIf(FIf fIf) {
            Optional<Statement> elze;
            if (fIf.getElse().isPresent())
                elze = Optional.of(enterStatement(fIf.getElse().get()));
            else
                elze = Optional.empty();
            return exitIf(fIf, exprVis.enterExpression(fIf.getCondition()), enterStatement(fIf.getThen()), elze);
        }

        @Override
        public Statement enterReturn(FReturn fReturn) {
            return exitReturn(fReturn, exprVis.enterExpression(fReturn.getExpression()));
        }

        @Override
        public Statement enterVarDeclaration(FVarDeclaration declaration) {
            Optional<Statement> value;
            if (declaration.getAssignment().isPresent())
                value = Optional.of(enterStatement(declaration.getAssignment().get()));
            else
                value = Optional.empty();
            return exitVarDeclaration(declaration, value);
        }

        @Override
        public Statement enterVarAssignment(FVarAssignment assignment) {
            return exitVarAssignment(assignment, exprVis.enterExpression(assignment.getValue()));
        }

        @Override
        public Statement enterWhile(FWhile fWhile) {
            return exitWhile(fWhile, exprVis.enterExpression(fWhile.getCondition()), enterStatement(fWhile.getBody()));
        }

        @Override
        public Statement enterFor(FFor fFor) {
            Optional<Statement> decl;
            if (fFor.getDeclaration().isPresent())
                decl = Optional.of(enterStatement(fFor.getDeclaration().get()));
            else
                decl = Optional.empty();
            Optional<Expression> cond;
            if (fFor.getCondition().isPresent())
                cond = Optional.of(exprVis.enterExpression(fFor.getCondition().get()));
            else
                cond = Optional.empty();
            Optional<Expression> inc;
            if (fFor.getIncrement().isPresent())
                inc = Optional.of(exprVis.enterExpression(fFor.getIncrement().get()));
            else
                inc = Optional.empty();
            return exitFor(fFor, decl, cond, inc, enterStatement(fFor.getBody()));
        }

        @Override
        public Statement enterForEach(FForEach forEach) {
            return exitForEach(forEach, exprVis.enterExpression(forEach.getContainer()), enterStatement(forEach.getBody()));
        }

        //bottom up
        @Override
        public Statement exitBlock(FBlock block, List<Statement> statements) {
            return getDefault();
        }

        @Override
        public Statement exitExpression(FExpressionStatement statement, Expression expression) {
            return getDefault();
        }

        @Override
        public Statement exitIf(FIf fIf, Expression cond, Statement then, Optional<Statement> elze) {
            return getDefault();
        }

        @Override
        public Statement exitReturn(FReturn fReturn, Expression value) {
            return getDefault();
        }

        @Override
        public Statement exitVarDeclaration(FVarDeclaration declaration, Optional<Statement> value) {
            return getDefault();
        }

        @Override
        public Statement exitVarAssignment(FVarAssignment assignment, Expression value) {
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
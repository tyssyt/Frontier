package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FFile;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.statement.FStatement;

public interface FullVisitor<File, Class, Field, Function, Statement, Expression> extends
        FileVisitor<File, Class>,
        ClassVisitor<Class, Field, Function, Statement>,
        StatementVisitor<Statement, Expression>,
        ExpressionVisitor<Expression>
{
    default File visit(FFile file) {
        return file.accept(this);
    }
    default Class visit(FClass clazz) {
        return clazz.accept(this);
    }
    default Statement visit(FStatement statement) {
        return statement.accept(this);
    }
    default Expression visit(FExpression expression) {
        return expression.accept(this);
    }
}

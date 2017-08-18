package tys.frontier.code.statement;

import tys.frontier.code.expression.FExpression;

public class FExpressionStatement implements FStatement {

    private FExpression expression;

    public FExpressionStatement(FExpression expression) {
        this.expression = expression;
    }

    public FExpression getExpression() {
        return expression;
    }
}

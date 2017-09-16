package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.expression.FExpression;

public class NonAssignableExpression extends SyntaxError {

    public final FExpression expression;

    public NonAssignableExpression(FExpression expression) {
        super(expression + "");
        this.expression = expression;
    }
}

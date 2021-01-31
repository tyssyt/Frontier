package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.expression.FExpression;

public class TypeDoesNotImplementFor extends SyntaxError {

    public final FExpression expression;

    public TypeDoesNotImplementFor(FExpression expression) {
        super(expression.getPosition(), expression.getType().getNamespace().getLocation().getPoint(), "Type: " + expression.getType() + " does not implement a for loop in expresssion: " + expression);
        this.expression = expression;
    }
}

package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.expression.FExpression;

public class InvalidForDeclaration extends SyntaxError {

    public final FExpression expression;

    public InvalidForDeclaration(String explanation, FExpression expression) {
        super(explanation + ": " + expression);
        this.expression = expression;
    }
}

package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.expression.FExpression;
import tys.frontier.parser.location.Position;

public class InvalidForDeclaration extends SyntaxError {

    public final FExpression expression;

    public InvalidForDeclaration(Position position, String explanation, FExpression expression) {
        super(position, explanation + ": " + expression);
        this.expression = expression;
    }
}

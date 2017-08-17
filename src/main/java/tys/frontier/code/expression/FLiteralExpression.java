package tys.frontier.code.expression;

import tys.frontier.code.literal.FLiteral;

public class FLiteralExpression implements FExpression {

    public final FLiteral literal;

    public FLiteralExpression(FLiteral literal) {
        this.literal = literal;
    }
}

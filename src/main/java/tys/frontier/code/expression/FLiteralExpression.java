package tys.frontier.code.expression;

import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.type.FType;

public class FLiteralExpression implements FExpression {

    public final FLiteral literal;

    public FLiteralExpression(FLiteral literal) {
        this.literal = literal;
    }

    @Override
    public FType getType() {
        return literal.getType();
    }
}

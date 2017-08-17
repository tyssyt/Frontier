package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.literal.FLiteral;

public class FLiteralExpression implements FExpression {

    public final FLiteral literal;

    public FLiteralExpression(FLiteral literal) {
        this.literal = literal;
    }

    @Override
    public FClass getType() {
        return literal.getType();
    }
}

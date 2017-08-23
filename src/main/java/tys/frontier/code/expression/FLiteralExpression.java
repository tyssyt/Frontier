package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.visitor.ExpressionVisitor;

public class FLiteralExpression implements FExpression {

    private FLiteral literal;

    public FLiteralExpression(FLiteral literal) {
        this.literal = literal;
    }

    @Override
    public FClass getType() {
        return literal.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.visitLiteral(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(literal);
    }
    @Override
    public String toString() {
        return tS();
    }
}

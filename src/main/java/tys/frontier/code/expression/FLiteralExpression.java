package tys.frontier.code.expression;

import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

public class FLiteralExpression implements FExpression {

    private FLiteral literal;

    public FLiteralExpression(FLiteral literal) {
        this.literal = literal;
    }

    public FLiteral getLiteral() {
        return literal;
    }

    @Override
    public FType getType() {
        return literal.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.visitLiteral(this);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitLiteral(this);
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

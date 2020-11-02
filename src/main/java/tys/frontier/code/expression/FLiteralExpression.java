package tys.frontier.code.expression;

import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;

public class FLiteralExpression extends FExpression {

    private FLiteral literal;

    public FLiteralExpression(Position position, FLiteral literal) {
        super(position);
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
}

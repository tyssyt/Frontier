package tys.frontier.code.expression;

import tys.frontier.code.Typed;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.type.FType;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.StringBuilderToString;

public abstract class FExpression implements Typed, StringBuilderToString {

    private Position position;

    protected FExpression(Position position) {
        this.position = position;
    }

    abstract public <E> E accept(ExpressionVisitor<E> visitor);
    abstract public <E> E accept(ExpressionWalker<E> walker);

    public Position getPosition() {
        return position;
    }

    public FExpression typeCheck(FType targetType) throws IncompatibleTypes {
        return FImplicitCast.create(position, targetType, this, Variance.Covariant);
    }

    @Override
    public String toString() {
        return tS();
    }
}

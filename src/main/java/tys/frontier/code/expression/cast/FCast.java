package tys.frontier.code.expression.cast;

import tys.frontier.code.expression.FExpression;
import tys.frontier.parser.location.Position;

public abstract class FCast extends FExpression {

    protected FExpression castedExpression;

    public FCast(Position position, FExpression castedExpression) {
        super(position);
        this.castedExpression = castedExpression;
    }

    public abstract boolean isNoOpCast();

    public FExpression getCastedExpression() {
        return castedExpression;
    }

    @Override
    public String toString() {
        return tS();
    }
}

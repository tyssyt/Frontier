package tys.frontier.code.expression.cast;

import tys.frontier.code.expression.FExpression;

public abstract class FCast implements FExpression {

    protected FExpression castedExpression;

    public FCast(FExpression castedExpression) {
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

package tys.frontier.code.expression.cast;

import tys.frontier.code.FType;
import tys.frontier.code.expression.FExpression;

public abstract class FCast implements FExpression {

    protected FType type;
    protected FExpression castedExpression;

    public FCast(FType type, FExpression castedExpression) {
        this.type = type;
        this.castedExpression = castedExpression;
    }

    @Override
    public FType getType() {
        return type;
    }

    public abstract boolean isNoOpCast();

    public FExpression getCastedExpression() {
        return castedExpression;
    }

    public void setCastedExpression(FExpression castedExpression) {
        this.castedExpression = castedExpression;
    }

    @Override
    public String toString() {
        return tS();
    }
}

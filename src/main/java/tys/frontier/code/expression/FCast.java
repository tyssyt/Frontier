package tys.frontier.code.expression;

import tys.frontier.code.FType;

public abstract class FCast implements FExpression {

    private FType type;
    private FExpression castedExpression;

    public FCast(FType type, FExpression castedExpression) {
        this.type = type;
        this.castedExpression = castedExpression;
    }

    @Override
    public FType getType() {
        return type;
    }

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

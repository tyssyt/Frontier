package tys.frontier.code.expression;

import tys.frontier.code.FClass;

public abstract class FCast implements FExpression {

    private FClass type;
    private FExpression castedExpression;

    public FCast(FClass type, FExpression castedExpression) {
        this.type = type;
        this.castedExpression = castedExpression;
    }

    @Override
    public FClass getType() {
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

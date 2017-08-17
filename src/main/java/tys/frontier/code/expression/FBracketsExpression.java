package tys.frontier.code.expression;

import tys.frontier.code.type.FType;

public class FBracketsExpression implements FExpression {

    public final FExpression inner;

    public FBracketsExpression(FExpression inner) {
        this.inner = inner;
    }

    @Override
    public FType getType() {
        return inner.getType();
    }
}

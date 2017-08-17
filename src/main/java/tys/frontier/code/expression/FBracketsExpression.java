package tys.frontier.code.expression;

import tys.frontier.code.FClass;

public class FBracketsExpression implements FExpression {

    public final FExpression inner;

    public FBracketsExpression(FExpression inner) {
        this.inner = inner;
    }

    @Override
    public FClass getType() {
        return inner.getType();
    }
}

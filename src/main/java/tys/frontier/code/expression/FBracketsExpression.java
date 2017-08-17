package tys.frontier.code.expression;

public class FBracketsExpression implements FExpression {

    public final FExpression inner;

    public FBracketsExpression(FExpression inner) {
        this.inner = inner;
    }
}

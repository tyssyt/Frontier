package tys.frontier.code.expression;

import tys.frontier.code.FVariable;

public class FVariableExpression implements FExpression {

    public final FVariable variable;

    public FVariableExpression(FVariable variable) {
        this.variable = variable;
    }
}

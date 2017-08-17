package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FVariable;

public class FVariableExpression implements FExpression {

    private final FVariable variable;

    public FVariableExpression(FVariable variable) {
        this.variable = variable;
    }

    @Override
    public FClass getType() {
        return variable.getType();
    }
}

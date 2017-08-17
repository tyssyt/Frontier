package tys.frontier.code.expression;

import tys.frontier.code.FVariable;
import tys.frontier.code.type.FType;

public class FVariableExpression implements FExpression {

    public final FVariable variable;

    public FVariableExpression(FVariable variable) {
        this.variable = variable;
    }

    @Override
    public FType getType() {
        return variable.getType();
    }
}

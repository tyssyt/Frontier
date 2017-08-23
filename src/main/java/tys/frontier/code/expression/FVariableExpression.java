package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FVariable;
import tys.frontier.code.visitor.ExpressionVisitor;

public class FVariableExpression implements FExpression {

    private final FVariable variable;

    public FVariableExpression(FVariable variable) {
        this.variable = variable;
    }

    @Override
    public FClass getType() {
        return variable.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.visitVariable(this);
    }
}

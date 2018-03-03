package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

public class FLocalVariableExpression implements FVariableExpression {

    private final FLocalVariable variable;

    public FLocalVariableExpression(FLocalVariable variable) {
        this.variable = variable;
    }

    @Override
    public FLocalVariable getVariable() {
        return variable;
    }

    @Override
    public FClass getType() {
        return variable.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.visitVariable(this);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitVariable(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(variable.getIdentifier());
    }

    @Override
    public String toString() {
        return tS();
    }
}

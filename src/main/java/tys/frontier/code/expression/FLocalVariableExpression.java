package tys.frontier.code.expression;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

public class FLocalVariableExpression implements FVariableExpression {

    private final FLocalVariable variable;
    private AccessType accessType = AccessType.LOAD;

    public FLocalVariableExpression(FLocalVariable variable) {
        assert variable != null;
        this.variable = variable;
    }

    @Override
    public FLocalVariableExpression copy() {
        return new FLocalVariableExpression(variable);
    }

    @Override
    public FLocalVariable getVariable() {
        return variable;
    }

    @Override
    public AccessType getAccessType() {
        return accessType;
    }

    @Override
    public void setAccessType(AccessType accessType) {
        assert this.accessType == AccessType.LOAD && accessType != AccessType.LOAD;
        this.accessType = accessType;
    }

    @Override
    public FType getType() {
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

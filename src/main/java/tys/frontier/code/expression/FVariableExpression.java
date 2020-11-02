package tys.frontier.code.expression;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;

public class FVariableExpression extends FExpression {

    public enum AccessType {
        LOAD,
        STORE
    }

    private final FLocalVariable variable;
    private AccessType accessType = AccessType.LOAD;

    public FVariableExpression(Position position, FLocalVariable variable) {
        super(position);
        assert variable != null;
        this.variable = variable;
    }

    public FVariableExpression copy() {
        return new FVariableExpression(null, variable); //TODO null or same pos?
    }

    public FLocalVariable getVariable() {
        return variable;
    }

    public AccessType getAccessType() {
        return accessType;
    }

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
}

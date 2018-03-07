package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FVariable;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

public class FFieldAccess implements FVariableExpression {

    private final FField field;
    private final FExpression object; //null for static fields
    private AccessType accessType = AccessType.LOAD;

    //for instance fields
    public FFieldAccess(FField field, FExpression object) {
        this.field = field;
        this.object = object;
    }

    //for static fields
    public FFieldAccess(FField field) {
        this.field = field;
        this.object = null;
    }

    public FField getField() {
        return field;
    }

    @Override
    public FVariable getVariable() {
        return field;
    }

    public FExpression getObject() {
        return object;
    }

    @Override
    public AccessType getAccessType() {
        return accessType;
    }

    @Override
    public void setStore() {
        assert accessType == AccessType.LOAD : "access type set twice: " + this;
        accessType = AccessType.STORE;
    }

    @Override
    public FClass getType() {
        return field.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterFieldAccess(this);
        E object = this.object == null ? null : this.object.accept(visitor);
        return visitor.exitFieldAccess(this, object);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitFieldAccess(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        if (object == null)
            sb.append(field.getClazz().getIdentifier());
        else
            object.toString(sb);
        return sb.append('.').append(field.getIdentifier());
    }
    @Override
    public String toString() {
        return tS();
    }
}

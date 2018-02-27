package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FVariable;
import tys.frontier.code.visitor.ExpressionVisitor;

public class FFieldAccess implements FVariableExpression {

    private FField field;
    private FExpression object;

    public FFieldAccess(FField field, FExpression object) {
        this.field = field;
        this.object = object;
    }

    public FFieldAccess(FField field) {
        assert(field.isStatic());
        this.field = field;
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
    public FClass getType() {
        return field.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterFieldAccess(this);
        return visitor.exitFieldAccess(this, visitor.visit(object));
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

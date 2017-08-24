package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.parser.syntaxTree.syntaxErrors.StaticAccessToInstanceField;

public class FFieldAccess implements FExpression {

    private FField field;
    private FExpression object;

    public FFieldAccess(FField field, FExpression object) {
        this.field = field;
        this.object = object;
    }

    public FFieldAccess(FField field) throws StaticAccessToInstanceField {
        if (!field.isStatic())
            throw new StaticAccessToInstanceField(field);
        this.field = field;
    }

    public FField getField() {
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
        return visitor.visitFieldAccess(this);
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

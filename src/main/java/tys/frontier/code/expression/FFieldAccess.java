package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.visitor.ExpressionVisitor;

public class FFieldAccess implements FExpression {

    private FField field;

    public FFieldAccess(FField field) {
        this.field = field;
    }

    @Override
    public FClass getType() {
        return field.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.visitFieldAccess(this);
    }
}

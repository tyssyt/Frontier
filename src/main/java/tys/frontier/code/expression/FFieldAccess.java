package tys.frontier.code.expression;

import tys.frontier.code.FField;
import tys.frontier.code.FType;
import tys.frontier.code.FVariable;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FFieldAccess implements FVariableExpression, HasInstanceObject {

    private final FField field;
    private FExpression object; //null for static fields
    private AccessType accessType = AccessType.LOAD;


    private FFieldAccess(FField field, FExpression object) {
        this.field = field;
        this.object = object;
        field.addAccess(this);
    }

    //for instance fields
    public static FFieldAccess createInstance(FField field, FExpression object) throws IncompatibleTypes {
        assert !field.isStatic();
        return new FFieldAccess(field, object).checkTypes();
    }
    public static FFieldAccess createInstanceTrusted(FField field, FExpression object) {
        try {
            return createInstance(field, object);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    //for static fields
    public static FFieldAccess createStatic(FField field) {
        assert field.isStatic();
        return new FFieldAccess(field, null);
    }

    @Override
    public FFieldAccess copy() {
        return new FFieldAccess(field, object);
    }

    public FField getField() {
        return field;
    }

    @Override
    public FVariable getVariable() {
        return field;
    }

    @Override
    public FExpression getObject() {
        return object;
    }

    @Override
    public void setObject(FExpression object) {
        this.object = object;
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
        return field.getType();
    }

    private FFieldAccess checkTypes() throws IncompatibleTypes {
        if (object != null)
            object = object.typeCheck(field.getMemberOf());
        return this;
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
    @SuppressWarnings("ResultOfMethodCallIgnored")
    public StringBuilder toString(StringBuilder sb) {
        if (object == null)
            sb.append(field.getMemberOf().getIdentifier());
        else
            object.toString(sb);
        return sb.append('.').append(field.getIdentifier());
    }
    @Override
    public String toString() {
        return tS();
    }
}

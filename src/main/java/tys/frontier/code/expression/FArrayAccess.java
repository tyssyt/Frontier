package tys.frontier.code.expression;

import tys.frontier.code.FVariable;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FArrayAccess implements FVariableExpression, HasInstanceObject {

    private FExpression array;
    private FExpression index;
    private AccessType accessType = AccessType.LOAD;

    private FArrayAccess(FExpression array, FExpression index) {
        this.array = array;
        this.index = index;
    }

    public static FArrayAccess create(FExpression array, FExpression index) throws IncompatibleTypes {
        return new FArrayAccess(array, index).checkTypes();
    }
    public static FArrayAccess createTrusted(FExpression array, FExpression index) {
        try {
            return create(array, index);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    @Override
    public FArrayAccess copy() {
        return new FArrayAccess(array, index);
    }

    @Override
    public FExpression getObject() {
        return array;
    }

    @Override
    public void setObject(FExpression object) {
        array = object;
    }

    public FExpression getIndex() {
        return index;
    }

    @Override
    public FVariable getVariable() {
        throw new RuntimeException("no idea what we should return here -.-"); //TODO maybe have a subclass that stores the actual (int) value that was accessed?
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
        return ((FArray) array.getType()).getBaseType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterArrayAccess(this);
        return visitor.exitArrayAccess(this, array.accept(visitor), index.accept(visitor));
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitArrayAccess(this);
    }

    private FArrayAccess checkTypes() throws IncompatibleTypes {
        index = index.typeCheck(FIntN._32);
        if (!(array.getType() instanceof FArray))
            throw new IncompatibleTypes(FArray.getArrayFrom(array.getType()), array.getType());
        return this;
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        array.toString(sb).append('[');
        return index.toString(sb).append(']');
    }
    @Override
    public String toString() {
        return tS();
    }
}

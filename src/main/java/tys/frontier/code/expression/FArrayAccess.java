package tys.frontier.code.expression;

import tys.frontier.code.FType;
import tys.frontier.code.FVariable;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class FArrayAccess implements FVariableExpression, HasInstanceObject, NeedsTypeCheck {

    private FExpression array;
    private FExpression index;
    private AccessType accessType = AccessType.LOAD;

    public FArrayAccess(FExpression array, FExpression index) {
        this.array = array;
        this.index = index;
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
        return ((FArray) array.getType()).getOneDimensionLess();
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

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (!(index.getType() == FIntN._32))
            index = new FImplicitCast(FIntN._32, index);
        if (!(array.getType() instanceof FArray))
            throw new IncompatibleTypes(FArray.getArrayFrom(array.getType(), 1), array.getType());
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

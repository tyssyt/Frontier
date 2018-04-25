package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FVariable;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class FArrayAccess implements FVariableExpression, NeedsTypeCheck {

    private final FExpression array;
    private final FExpression index;
    private AccessType accessType = AccessType.LOAD;

    public FArrayAccess(FExpression array, FExpression index) throws IncompatibleTypes {
        this.array = array;
        this.index = index;
        checkTypes();
    }

    public FExpression getArray() {
        return array;
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
    public void setStore() {
        assert accessType == AccessType.LOAD : "access type set twice: " + this;
        accessType = AccessType.STORE;
    }

    @Override
    public FClass getType() {
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
            throw new IncompatibleTypes(FIntN._32, index.getType());
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

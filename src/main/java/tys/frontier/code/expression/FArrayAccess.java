package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class FArrayAccess implements FExpression, NeedsTypeCheck {

    private FExpression array;
    private FExpression index;

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
    public FClass getType() {
        return ((FArray) array.getType()).getOneDimensionLess();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.enterArrayAccess(this);
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (!FPredefinedClass.intTypes.contains(index.getType()))
            throw new IncompatibleTypes(FPredefinedClass.intTypes.iterator().next(), index.getType());
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

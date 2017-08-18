package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.code.statement.NeedsTypeCheck;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

public class FArrayAccess implements FExpression, NeedsTypeCheck {

    private FExpression array;
    private FExpression index;

    public FArrayAccess(FExpression array, FExpression index) throws IncompatibleTypes {
        this.array = array;
        this.index = index;
        checkTypes();
    }

    @Override
    public FClass getType() {
        return ((FArray) array.getType()).getOneDimensionLess();
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (!FPredefinedClass.intTypes.contains(index.getType()))
            throw new IncompatibleTypes(FPredefinedClass.intTypes.iterator().next(), index.getType());
    }
}

package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FArray;

public class FArrayAccess implements FExpression {

    private FExpression array;
    private FExpression index; //TODO int

    @Override
    public FClass getType() {
        return ((FArray) array.getType()).getBaseClass();
    }
}

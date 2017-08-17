package tys.frontier.code.expression;

import tys.frontier.code.type.FArrayType;
import tys.frontier.code.type.FType;

public class FArrayAccess implements FExpression {

    private FExpression array;
    private FExpression index; //TODO int

    @Override
    public FType getType() {
        return ((FArrayType) array.getType()).baseType;
    }
}

package tys.frontier.code.typeInference;

import tys.frontier.code.expression.FExpression;

public abstract class TypeConstraint {

    private FExpression origin;

    public TypeConstraint(FExpression origin) {
        this.origin = origin;
    }

    public FExpression getOrigin() {
        return origin;
    }

    public void setOrigin(FExpression origin) {
        this.origin = origin;
    }

    @Override
    abstract public int hashCode();
    @Override
    abstract public boolean equals(Object obj);
}

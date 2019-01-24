package tys.frontier.code.typeInference;

import tys.frontier.code.FType;
import tys.frontier.code.expression.FExpression;

public class IsType extends TypeConstraint {

    private FType target;

    public IsType(FExpression origin, FType target) {
        super(origin);
        this.target = target;
    }

    public FType getTarget() {
        return target;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IsType)) return false;

        IsType isType = (IsType) o;

        return target.equals(isType.target);
    }

    @Override
    public int hashCode() {
        return target.hashCode();
    }
}

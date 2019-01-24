package tys.frontier.code.typeInference;

import tys.frontier.code.FType;
import tys.frontier.code.expression.FExpression;

public class ImplicitCastable extends TypeConstraint {

    private FType to;

    public ImplicitCastable(FExpression origin, FType to) {
        super(origin);
        this.to = to;
    }

    public FType getTo() {
        return to;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ImplicitCastable)) return false;

        ImplicitCastable that = (ImplicitCastable) o;

        return to.equals(that.to);
    }

    @Override
    public int hashCode() {
        return to.hashCode()*31;
    }
}
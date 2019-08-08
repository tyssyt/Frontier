package tys.frontier.code.typeInference;

import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.util.Utils;

public class ImplicitCastable extends TypeConstraint {

    private FType target;
    private Variance variance;

    public ImplicitCastable(Object origin, FType target, Variance variance) {
        super(origin);
        if (target instanceof FTypeVariable && ((FTypeVariable) target).getConstraints().isResolved())
            this.target = ((FTypeVariable) target).getConstraints().getResolved();
        else
            this.target = target;
        if (variance == Variance.Contravariant && !target.canImplicitlyCast())
            this.variance = Variance.Invariant;
        else
            this.variance = variance;
    }

    public FType getTarget() {
        return target;
    }

    public Variance getVariance() {
        return variance;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ImplicitCastable)) return false;

        ImplicitCastable that = (ImplicitCastable) o;

        if (!target.equals(that.target)) return false;
        return variance == that.variance;
    }

    @Override
    public int hashCode() {
        int result = target.hashCode();
        result = 31 * result + variance.hashCode();
        return result;
    }

    @Override
    public String toString() {
        switch (variance) {
            case Covariant:     return "> " + target.getIdentifier().name;
            case Contravariant: return "< " + target.getIdentifier().name;
            case Invariant:     return "= " + target.getIdentifier().name;
            default:            return Utils.cantHappen();
        }

    }
}

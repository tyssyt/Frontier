package tys.frontier.code.typeInference;

import tys.frontier.code.type.FType;

public class ImplicitCastable extends TypeConstraint {

    private FType target;
    private Variance variance;

    public ImplicitCastable(Object origin, FType target, Variance variance) {
        super(origin);
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
        return switch (variance) {
            case Covariant -> "> " + target.getIdentifier().name;
            case Contravariant -> "< " + target.getIdentifier().name;
            case Invariant -> "= " + target.getIdentifier().name;
        };

    }
}

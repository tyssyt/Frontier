package tys.frontier.code.typeInference;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;

import java.util.Iterator;
import java.util.Map;

public class ImplicitCastable extends TypeConstraint {

    private FType target;
    private Variance variance;

    public ImplicitCastable(Object origin, FType target, Variance variance) {
        super(origin);
        if (target instanceof FTypeVariable && ((FTypeVariable) target).isResolved())
            this.target = ((FTypeVariable) target).getResolved();
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

    public static ImplicitCastable removeSatisfiableCheckUnsatisfiable(ListMultimap<FTypeVariable, ImplicitCastable> constraints) {
        Iterator<Map.Entry<FTypeVariable, ImplicitCastable>> it = constraints.entries().iterator();
        while (it.hasNext()) {
            Map.Entry<FTypeVariable, ImplicitCastable> entry = it.next();
            if (entry.getKey().getConstraints().satisfies(entry.getValue())) {
                it.remove(); //remove all satisfied constraints
                continue;
            }
            if (entry.getKey().isFixed()) {
                return entry.getValue(); //constraint is unsatisfiable
            }
            //otherwise the constraint stays in the set
        }
        return null;
    }

    public static void addAll(Multimap<FTypeVariable, ImplicitCastable> constraints) throws UnfulfillableConstraints {
        for (Map.Entry<FTypeVariable, ImplicitCastable> entry : constraints.entries()) {
            if (!entry.getKey().tryAddConstraint(entry.getValue()))
                throw new UnfulfillableConstraints(entry.getKey().getConstraints(), entry.getValue(), null); //TODO
        }
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

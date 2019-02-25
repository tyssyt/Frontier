package tys.frontier.code.typeInference;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.FClass;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Utils;

import java.util.*;

import static tys.frontier.code.typeInference.Variance.Invariant;

public class TypeConstraints { //TODO there is a lot of potential for optimization in here

    //empty immutable constraints
    public static TypeConstraints EMPTY = new TypeConstraints(Collections.emptySet()) {
        //TODO

        @Override
        public void add(TypeConstraint constraint) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean isConsistent() {
            return true;
        }

        @Override
        public boolean satisfies(TypeConstraint constraint) {
            return false;
        }

        @Override
        public TypeConstraints copy() {
            return create();
        }
    };

    private Set<TypeConstraint> constraints;

    private TypeConstraints(Set<TypeConstraint> constraints) {
        this.constraints = constraints;
    }

    public static TypeConstraints create() {
        return new TypeConstraints(new HashSet<>());
    }

    public TypeConstraints copy() {
        return new TypeConstraints(new HashSet<>(constraints));
    }

    public boolean isEmpty() {
        return constraints.isEmpty();
    }

    public void add(TypeConstraint constraint) {
        if (this.satisfies(constraint))
            return;
        if (constraint instanceof ImplicitCastable) { //remove all constraints implied by the new one
            ImplicitCastable implicitCastable = (ImplicitCastable) constraint;
            Multimap<FTypeVariable, TypeConstraint> newConstraints = ArrayListMultimap.create();
            TypeConstraint c = null;
            for (Iterator<TypeConstraint> it = constraints.iterator(); it.hasNext(); c=it.next()) {
                if (implies(implicitCastable, c, newConstraints) && newConstraints.isEmpty())
                    it.remove();
                newConstraints.clear();
            }
        }
        constraints.add(constraint);
    }

    public void addAll(Collection<TypeConstraint> newConstraints) {
        for (TypeConstraint constraint : newConstraints) {
            add(constraint);
        }
    }

    public boolean isConsistent() {
        return true; //TODO
    }

    public boolean satisfies(TypeConstraint constraint) {
        if (constraints.contains(constraint))
            return true;
        if (constraint instanceof ImplicitCastable) {
            ImplicitCastable implicitCastable = (ImplicitCastable) constraint;
            Multimap<FTypeVariable, TypeConstraint> newConstraints = ArrayListMultimap.create();
            for (TypeConstraint c : constraints) {
                if (c instanceof ImplicitCastable && implies((ImplicitCastable) c, implicitCastable, newConstraints) && newConstraints.isEmpty())
                    return true;
                newConstraints.clear();
            }
            return false;
        } else if (constraint instanceof HasCall) {
            HasCall hasCall = (HasCall) constraint;
            Multimap<FTypeVariable, TypeConstraint> newConstraints = ArrayListMultimap.create();
            for (TypeConstraint c : constraints) {
                if (c instanceof ImplicitCastable && implies((ImplicitCastable) c, hasCall, newConstraints) && newConstraints.isEmpty())
                    return true;
                newConstraints.clear();
            }
            return false;
        } else {
            return Utils.cantHappen();
        }
    }

    public FClass resolve() throws UnfulfillableConstraints {  //this will slowly become more powerful, as I get more knowledgeable about how this should properly behave
        ImplicitCastable res = null;
        int foundCandidates = 0;
        for (TypeConstraint constraint : constraints) {
            if (constraint instanceof ImplicitCastable) {
                ImplicitCastable implicitCastable = (ImplicitCastable) constraint;
                if (implicitCastable.getTarget() instanceof FClass) {
                    res = implicitCastable;
                    foundCandidates++;
                    if (implicitCastable.getVariance() == Invariant) {
                        foundCandidates = 1;
                        break;
                    }
                }
            }
        }
        if (foundCandidates != 1)
            return Utils.NYI("constraint resolving");
        if (res.getVariance() != Invariant)
            res = new ImplicitCastable(res.getOrigin(), res.getTarget(), Invariant);
        Multimap<FTypeVariable, TypeConstraint> newConstraints = ArrayListMultimap.create();
        for (TypeConstraint constraint : constraints) {
            if (!implies(res, constraint, newConstraints) || !newConstraints.isEmpty()) {
                throw new UnfulfillableConstraints(null, this, res, constraint); //TODO where do we get the var from?
            }
        }
        return (FClass) res.getTarget();
    }

    public static boolean implies(ImplicitCastable a, TypeConstraint b, Multimap<FTypeVariable, TypeConstraint> newConstraints) {
        if (b instanceof ImplicitCastable)
            return implies(a, (ImplicitCastable) b, newConstraints);
        else if (b instanceof HasCall)
            return implies(a, (HasCall) b, newConstraints);
        else
            return Utils.cantHappen();
    }

    public static boolean implies(ImplicitCastable a, ImplicitCastable b, Multimap<FTypeVariable, TypeConstraint> newConstraints) {
        if (a.getVariance().sign * b.getVariance().sign == -1) //a covariant and b contravariant or vice versa
            return false;

        try {
            if (a.getTarget() == b.getTarget())
                return true;
            ImplicitTypeCast.create(a.getTarget(), b.getTarget(), b.getVariance(), newConstraints);
            return true;
        } catch (IncompatibleTypes incompatibleTypes) {
            return false;
        }
    }

    public static boolean implies(ImplicitCastable a, HasCall b, Multimap<FTypeVariable, TypeConstraint> newConstraints) {
        try {
            //TODO I have no Idea how/if the variance of a should be considered in resolving
            a.getTarget().resolveFunction(b.getIdentifier(), b.getArguments(), b.getTypeInstantiation(), newConstraints);
            return true;
        } catch (FunctionNotFound functionNotFound) {
            return false;
        }
    }
}

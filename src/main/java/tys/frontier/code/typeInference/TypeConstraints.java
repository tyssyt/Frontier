package tys.frontier.code.typeInference;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Multimap;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;

import static tys.frontier.code.typeInference.Variance.Contravariant;
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

    private boolean resolved = false; //debug flag, only used in assertion

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
        assert !resolved;
        if (constraint instanceof ImplicitCastable) { //remove all constraints implied by the new one
            ImplicitCastable implicitCastable = (ImplicitCastable) constraint;
            Multimap<FTypeVariable, TypeConstraint> newConstraints = ArrayListMultimap.create();
            for (Iterator<TypeConstraint> it = constraints.iterator(); it.hasNext();) {
                TypeConstraint c = it.next();
                if (implies(implicitCastable, c, newConstraints) && newConstraints.isEmpty())
                    it.remove();
                if (c instanceof ImplicitCastable && ((ImplicitCastable) c).getTarget() == implicitCastable.getTarget()) {
                    //by process of elimination, we know that the 2 constraints are co- & contravariant, so combine them to invariant
                    it.remove();
                    implicitCastable = new ImplicitCastable(new Pair<>(implicitCastable, c), implicitCastable.getTarget(), Invariant);
                }
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

    public FType resolve(Multimap<FTypeVariable, TypeConstraint> newConstraints) throws UnfulfillableConstraints {  //this will slowly become more powerful, as I get more knowledgeable about how this should properly behave
        assert !resolved;
        FType proposition = proposeType();
        ImplicitCastable typeConstraint = new ImplicitCastable(this, proposition, Invariant);
        for (TypeConstraint constraint : constraints) {
            if (!implies(typeConstraint, constraint, newConstraints)) {
                throw new UnfulfillableConstraints(null, this, typeConstraint, constraint); //TODO where do we get the var from?
            }
        }
        resolved = true;
        return proposition;
    }

    private FType proposeType() throws UnfulfillableConstraints {
        List<FType> fullyInstantiated = new ArrayList<>();
        List<FType> rest = new ArrayList<>();

        for (TypeConstraint constraint : constraints) {
            if(constraint instanceof ImplicitCastable) {
                FType target = ((ImplicitCastable) constraint).getTarget();
                if (target.isFullyInstantiated()) {
                    fullyInstantiated.add(target);
                } else {
                    rest.add(target);
                }
            }
        }

        if (fullyInstantiated.size() > 0) {

            if (fullyInstantiated.size() == 1)
                return Iterables.getOnlyElement(fullyInstantiated);

            return Utils.NYI("constraint resolving with 2 fully Instantiated classes");
        }

        if (rest.size() > 0) {

            if (rest.size() == 1)
                return Iterables.getOnlyElement(rest);

            return Utils.NYI("constraint resolving with more then 1 candidate");
        }
        throw new UnfulfillableConstraints(null, this, null, null); //TODO Maybe we should create a different error type for this
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
        if (a.getVariance() == Contravariant) //casts from constraints cannot be used for function resolving
            return false;
        try {
            //TODO I have no Idea how/if the variance of a should be considered in resolving
            a.getTarget().resolveFunction(b.getIdentifier(), b.getArgumentTypes(), b.getTypeInstantiation(), newConstraints);
            return true;
        } catch (FunctionNotFound functionNotFound) {
            return false;
        }
    }
}

package tys.frontier.code.typeInference;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

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
            return this;
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

    public boolean isConsistent() {
        return false; //TODO
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

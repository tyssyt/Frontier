package tys.frontier.code.typeInference;

import tys.frontier.util.Utils;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class TypeConstraints {

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
        public boolean makesInconsistent(TypeConstraint constraint) {
            return Utils.cantHappen();
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
        constraints.add(constraint);
    }

    public boolean isConsistent() {
        return false; //TODO
    }

    public boolean satisfies(TypeConstraint constraint) {
        if (constraints.contains(constraint))
            return true;
        if (constraint instanceof ImplicitCastable) {
            return false; //TODO
        } else if (constraint instanceof HasCall) {
            return Utils.NYI("satisfyability checks for HasCall");
        } else {
            return Utils.cantHappen();
        }
    }

    public boolean makesInconsistent(TypeConstraint constraint) {
        return true; //TODO
    }
}

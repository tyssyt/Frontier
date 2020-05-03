package tys.frontier.code.expression.cast;

import com.google.common.collect.Multimap;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;

import static tys.frontier.code.typeInference.Variance.*;

public class TypeVariableCast extends ImplicitTypeCast {

    private TypeVariableCast(FType base, FType target, Variance variance) {
        super(base, target, variance);
    }

    public static TypeVariableCast createTVC(FType baseType, FType targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) {
        assert baseType instanceof FTypeVariable || targetType instanceof FTypeVariable;

        //no need to add constraints for T -> T?
        if (variance == Covariant && targetType instanceof FOptional && ((FOptional) targetType).getBaseType() == baseType)
            return new TypeVariableCast(baseType, targetType, variance);
        if (variance == Contravariant && baseType instanceof FOptional && ((FOptional) baseType).getBaseType() == targetType)
            return new TypeVariableCast(baseType, targetType, variance);

        if (baseType instanceof FTypeVariable && targetType instanceof FTypeVariable) {
            FTypeVariable baseTypeVar = (FTypeVariable) baseType;
            FTypeVariable targetTypeVar = (FTypeVariable) targetType;

            if (baseTypeVar.isFixed() ^ ((FTypeVariable) targetType).isFixed()) {
                if (baseTypeVar.isFixed())
                    fixedVsNonFixedTypeCast(baseTypeVar, targetTypeVar, variance, constraints);
                else
                    fixedVsNonFixedTypeCast(targetTypeVar, baseTypeVar, variance.opposite(), constraints);
                return new TypeVariableCast(baseType, targetType, variance);
            } //TODO now that we made this special case, we can consider never creating constraints for fixed vars, aka handling the both fixed and one fixed and other is not a type var cases as well
        }

        if (baseType instanceof FTypeVariable) {
            FTypeVariable b = (FTypeVariable) baseType;
            constraints.put(b, new ImplicitCastable(null, targetType, variance));
        }
        if (targetType instanceof FTypeVariable) {
            FTypeVariable t = (FTypeVariable) targetType;
            constraints.put(t, new ImplicitCastable(null, baseType, variance.opposite()));
        }
        return new TypeVariableCast(baseType, targetType, variance);
    }

    public static void fixedVsNonFixedTypeCast(FTypeVariable fixed, FTypeVariable nonFixed, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) {
        ImplicitCastable constraint = new ImplicitCastable(null, nonFixed, variance);
        if (fixed.getConstraints().satisfies(constraint)) {
            constraints.put(nonFixed, new ImplicitCastable(null, fixed, variance.opposite()));
        } else { //fixed var can't satisfy constraint, force nonFixed invariant fixed
            constraints.put(nonFixed, new ImplicitCastable(null, fixed, Invariant));
        }
    }

    @Override
    public long getCost() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean isNoOpCast() {
        return false;
    }
}

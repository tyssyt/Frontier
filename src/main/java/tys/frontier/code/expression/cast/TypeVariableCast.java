package tys.frontier.code.expression.cast;

import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.Constraints;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.Variance;

import static tys.frontier.code.typeInference.Variance.Contravariant;
import static tys.frontier.code.typeInference.Variance.Covariant;

public class TypeVariableCast extends ImplicitTypeCast {

    private TypeVariableCast(FType base, FType target, Variance variance) {
        super(base, target, variance);
    }

    public static TypeVariableCast createTVC(FType baseType, FType targetType, Variance variance, Constraints constraints) {
        assert baseType instanceof FTypeVariable || targetType instanceof FTypeVariable;

        //no need to add constraints for T -> T?
        if (variance == Covariant && targetType instanceof FOptional && ((FOptional) targetType).getBaseType() == baseType)
            return new TypeVariableCast(baseType, targetType, variance);
        if (variance == Contravariant && baseType instanceof FOptional && ((FOptional) baseType).getBaseType() == targetType)
            return new TypeVariableCast(baseType, targetType, variance);

        if (baseType instanceof FTypeVariable) {
            FTypeVariable b = (FTypeVariable) baseType;
            constraints.add(b, new ImplicitCastable(null, targetType, variance));
        }
        if (targetType instanceof FTypeVariable) {
            FTypeVariable t = (FTypeVariable) targetType;
            constraints.add(t, new ImplicitCastable(null, baseType, variance.opposite()));
        }
        return new TypeVariableCast(baseType, targetType, variance);
    }

    @Override
    public long getCost() {
        return 640;
    }

    @Override
    public boolean isNoOpCast() {
        return false;
    }
}

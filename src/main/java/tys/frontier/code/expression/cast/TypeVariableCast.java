package tys.frontier.code.expression.cast;

import com.google.common.collect.Multimap;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;

public class TypeVariableCast extends ImplicitTypeCast {

    private TypeVariableCast(FType base, FType target, Variance variance) {
        super(base, target, variance);
    }

    public static TypeVariableCast createTVC(FType baseType, FType targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) {
        assert baseType instanceof FTypeVariable || targetType instanceof FTypeVariable;
        if (baseType instanceof FTypeVariable) {
            FTypeVariable b = (FTypeVariable) baseType;
            constraints.put(b, new ImplicitCastable(null, targetType, variance));
        }
        if (targetType instanceof FTypeVariable) {
            FTypeVariable t = (FTypeVariable) targetType;
            constraints.put(t, new ImplicitCastable(null, baseType, Variance.Contravariant.then(variance)));
        }
        return new TypeVariableCast(baseType, targetType, variance);
    }

    @Override
    public int getCost() {
        return 0; //TODO cant give a definitive answer here...
    }

    @Override
    public boolean isNoOpCast() {
        return false;
    }
}

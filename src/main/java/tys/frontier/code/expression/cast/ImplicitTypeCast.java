package tys.frontier.code.expression.cast;

import com.google.common.collect.Multimap;
import tys.frontier.code.FClass;
import tys.frontier.code.FInstantiatedClass;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

import static tys.frontier.code.typeInference.Variance.Invariant;

public abstract class ImplicitTypeCast {

    protected FType base;
    protected FType target;
    protected Variance variance;

    protected ImplicitTypeCast(FType base, FType target, Variance variance) {
        this.base = base;
        this.target = target;
        this.variance = variance;
    }

    public static ImplicitTypeCast create(FType baseType, FType targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        assert baseType != targetType;

        //first check if either base or targetType is a TypeVariable and do a type variable cast
        if (baseType instanceof FTypeVariable || targetType instanceof FTypeVariable) {
            return TypeVariableCast.createTVC(baseType, targetType, variance, constraints);
        }

        //if we are invariant, there can't be an implicit cast
        if (variance == Invariant)
            throw new IncompatibleTypes(targetType, baseType);

        if (targetType instanceof FInstantiatedClass && baseType instanceof FInstantiatedClass && ((FInstantiatedClass) targetType).getBaseClass() == ((FInstantiatedClass) baseType).getBaseClass())
            return TypeParameterCast.createTPC((FInstantiatedClass) baseType, (FInstantiatedClass) targetType, variance, constraints); //TODO what if one of them is the base class, I think thats in theory possible
        if (targetType instanceof FOptional && baseType instanceof FOptional)
            return TypeParameterCast.createTPC((FOptional) baseType, (FOptional) targetType, variance, constraints); //TODO optional will be made generic some day
        if (targetType instanceof FFunctionType && baseType instanceof FFunctionType)
            return TypeParameterCast.createTPC((FFunctionType) baseType, (FFunctionType) targetType, variance, constraints); //TODO function types will be made generic some day
        return TypeConversion.createTC((FClass) baseType, (FClass) targetType, variance, constraints);
    }




    public FType getBase() {
        return base;
    }

    public FType getTarget() {
        return target;
    }

    public Variance getVariance() {
        return variance;
    }

    public abstract int getCost();

    public abstract boolean isNoOpCast();
}

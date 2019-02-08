package tys.frontier.code.expression.cast;

import com.google.common.collect.Multimap;
import tys.frontier.code.FClass;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import static tys.frontier.code.typeInference.Variance.Contravariant;
import static tys.frontier.code.typeInference.Variance.Covariant;

public class TypeConversion extends ImplicitTypeCast {

    public enum CastType {
        INTEGER_PROMOTION(0, 1),
        FLOAT_PROMOTION(32, 0),
        TO_OPTIONAL(32, 0),
        OPTIONAL_TO_BOOL(32, 0), //this is a special case and should never be done as an inner cast
        DELEGATE(0, 32);

        public final int baseCost;
        public final int costPerStep;

        CastType(int baseCost, int costPerStep) {
            this.baseCost = baseCost;
            this.costPerStep = costPerStep;
        }

        public int getCost(int steps) {
            return baseCost + steps * costPerStep;
        }
    }

    private CastType castType;
    private TypeParameterCast inner;

    private TypeConversion(FClass base, FClass target, Variance variance, CastType castType, TypeParameterCast inner) {
        super(base, target, variance);
        this.castType = castType;
        this.inner = inner;
    }

    public static TypeConversion createTC(FClass baseType, FClass targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes { //TODO I do everything twice in here, which sucks and is error prone
        assert variance == Covariant || variance == Contravariant;
        assert baseType != targetType;

        if (targetType instanceof FIntN && baseType instanceof FIntN) {
            if (Integer.compare(((FIntN) targetType).getN(), ((FIntN) baseType).getN()) * variance.sign > 0) //TODO this is pure guesswork, verify pls
                return new TypeConversion(baseType, targetType, variance, CastType.INTEGER_PROMOTION, null);
            else
                throw new IncompatibleTypes(targetType, baseType);
        }
        //TODO upwards float cast
        if (variance == Covariant && targetType instanceof FOptional) {
            FType targetBase = ((FOptional) targetType).getBaseType();
            if (baseType != targetBase) { //if the inner type doesn't match, try casting it, because optionals are covariant
                TypeParameterCast inner = (TypeParameterCast) ImplicitTypeCast.create(baseType, targetBase, variance, constraints);
                return new TypeConversion(baseType, targetType, variance, CastType.TO_OPTIONAL, inner);
            }
            return new TypeConversion(baseType, targetType, variance, CastType.TO_OPTIONAL, null);
        }
        if (variance == Contravariant && baseType instanceof FOptional) {
            FType baseBase = ((FOptional) baseType).getBaseType();
            if (targetType != baseBase) { //if the inner type doesn't match, try casting it, because optionals are covariant
                TypeParameterCast inner = (TypeParameterCast) ImplicitTypeCast.create(baseBase, targetType, variance, constraints);
                return new TypeConversion(baseType, targetType, variance, CastType.TO_OPTIONAL, inner);
            }
            return new TypeConversion(baseType, targetType, variance, CastType.TO_OPTIONAL, null);
        }
        if (variance == Covariant && baseType instanceof FOptional && targetType == FBool.INSTANCE)
            return new TypeConversion(baseType, targetType, variance, CastType.OPTIONAL_TO_BOOL, null);
        if (variance == Contravariant && targetType instanceof FOptional && baseType == FBool.INSTANCE)
            return new TypeConversion(baseType, targetType, variance, CastType.OPTIONAL_TO_BOOL, null);
        if (variance == Covariant && baseType.getDelegate(targetType) != null)
            return new TypeConversion(baseType, targetType, variance, CastType.DELEGATE, null); //TODO there are more complex cast paths here, base could be instantiated and the instantiation delegates to target
        if (variance == Contravariant && targetType.getDelegate(baseType) != null)
            return new TypeConversion(baseType, targetType, variance, CastType.DELEGATE, null); //TODO there are more complex cast paths here

        throw new IncompatibleTypes(targetType, baseType);
    }

    public CastType getCastType() {
        return castType;
    }

    public TypeParameterCast getInner() {
        return inner;
    }

    @Override
    public FClass getBase() {
        return (FClass) base;
    }

    @Override
    public FClass getTarget() {
        return (FClass) target;
    }

    @Override
    public int getCost() {
        int i = inner == null ? 0 : inner.getCost();
        switch (castType) {
            case INTEGER_PROMOTION:
                return i + castType.getCost(variance.sign * (((FIntN) target).getN() - ((FIntN) base).getN()));
            case FLOAT_PROMOTION:
                return i + castType.getCost(0);
            case TO_OPTIONAL:
                return i + castType.getCost(0);
            case OPTIONAL_TO_BOOL:
                return i + castType.getCost(0);
            case DELEGATE:
                if (variance == Covariant)
                    return i + castType.getCost(32 * getBase().getDelegate(target).size());
                else
                    return i + castType.getCost(32 * getTarget().getDelegate(base).size());
            default:
                return Utils.cantHappen();
        }
    }

    @Override
    public boolean isNoOpCast() {
        switch (castType) {
            case INTEGER_PROMOTION:
                return false;
            case FLOAT_PROMOTION:
                return false;
            case TO_OPTIONAL:
                return base != FBool.INSTANCE && target != FBool.INSTANCE && (inner == null || inner.isNoOpCast());
            case OPTIONAL_TO_BOOL:
                return false;
            case DELEGATE:
                return false;
            default:
                return Utils.cantHappen();
        }
    }
}

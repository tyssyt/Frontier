package tys.frontier.code.expression.cast;

import tys.frontier.code.FField;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.typeInference.Constraints;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Pair;

import java.util.Optional;

import static tys.frontier.code.typeInference.Variance.Contravariant;
import static tys.frontier.code.typeInference.Variance.Covariant;

public class TypeConversion extends ImplicitTypeCast {

    public enum CastType {
        INTEGER_PROMOTION(0, 1),
        FLOAT_PROMOTION(32, 0),
        TO_OPTIONAL(32, 0),
        OPTIONAL_TO_BOOL(3200, 0), //this is a special case and should never be done as an inner cast
        DELEGATE(32, 0);

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
    private ImplicitTypeCast inner;

    private TypeConversion(FClass base, FClass target, Variance variance, CastType castType, ImplicitTypeCast inner) {
        super(base, target, variance);
        this.castType = castType;
        this.inner = inner;
    }

    public static Optional<TypeConversion> tryTupleToOptional(FTuple baseType, FTuple targetType, Variance variance) {
        if (variance == Covariant && FOptional.canBeTreatedAsOptional(targetType) && FOptional.from(baseType) == targetType)
            return Optional.of(new TypeConversion(baseType, targetType, variance, CastType.TO_OPTIONAL, null));
        if (variance == Contravariant && FOptional.canBeTreatedAsOptional(baseType) && FOptional.from(targetType) == baseType)
            return Optional.of(new TypeConversion(baseType, targetType, variance, CastType.TO_OPTIONAL, null));
        return Optional.empty();
    }

    public static TypeConversion createTC(FClass baseType, FClass targetType, Variance variance, Constraints constraints) throws IncompatibleTypes { //TODO I do everything twice in here, which sucks and is error prone
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
                ImplicitTypeCast inner = ImplicitTypeCast.create(baseType, targetBase, variance, constraints);
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
        if (variance == Covariant) {
            Pair<FField, ImplicitTypeCast> delegate = baseType.getDelegate(targetType, constraints);
            if (delegate != null) {
                ImplicitTypeCast outer = delegate.b;
                return new TypeConversion(baseType, targetType, variance, CastType.DELEGATE, outer);
            }
        }
        if (variance == Contravariant) {
            Pair<FField, ImplicitTypeCast> delegate = targetType.getDelegate(baseType, constraints);
            if (delegate != null)
                return new TypeConversion(baseType, targetType, variance, CastType.DELEGATE, delegate.b);
        }
        throw new IncompatibleTypes(targetType, baseType);
    }

    public CastType getCastType() {
        return castType;
    }

    public ImplicitTypeCast getInner() {
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
    public long getCost() {
        long l = inner == null ? 0 : inner.getCost();
        return switch (castType) {
            case FLOAT_PROMOTION, TO_OPTIONAL, OPTIONAL_TO_BOOL, DELEGATE -> l + castType.getCost(1);
            case INTEGER_PROMOTION -> l + castType.getCost(variance.sign * (((FIntN) target).getN() - ((FIntN) base).getN()));
        };
    }

    @Override
    public boolean isNoOpCast() {
        return switch (castType) {
            case INTEGER_PROMOTION, FLOAT_PROMOTION, OPTIONAL_TO_BOOL, DELEGATE -> false;
            case TO_OPTIONAL -> base != FBool.INSTANCE && target != FBool.INSTANCE && (inner == null || inner.isNoOpCast());
        };
    }
}

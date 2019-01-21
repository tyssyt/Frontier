package tys.frontier.code.expression.cast;

import tys.frontier.code.FClass;
import tys.frontier.code.FType;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import static tys.frontier.code.typeInference.Variance.Contravariant;
import static tys.frontier.code.typeInference.Variance.Covariant;

public class DirectConversion extends FImplicitCast {

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

    private DirectConversion(FType type, FExpression castedExpression, CastType castType, Variance variance) {
        super(type, castedExpression, variance);
        this.castType = castType;
    }

    public static DirectConversion create(FType type, FExpression castedExpression, Variance variance) throws IncompatibleTypes {
        assert variance == Covariant || variance == Contravariant;
        FType baseType, targetType;
        if (variance == Covariant) {
            baseType = castedExpression.getType();
            targetType = type;
        } else {
            baseType = type;
            targetType = castedExpression.getType();
        }

        if (targetType instanceof FIntN && baseType instanceof FIntN &&
                ((FIntN) targetType).getN() > ((FIntN) baseType).getN())
            return new DirectConversion(type, castedExpression, CastType.INTEGER_PROMOTION, variance);
        //TODO upwards float cast
        if (targetType instanceof FOptional) {
            FType targetBase = ((FOptional) targetType).getBaseType();
            if (baseType != targetBase) { //if the inner type doesn't match, try casting it, because optionals are covariant
                if (variance == Covariant)
                    castedExpression = FImplicitCast.create(baseType, castedExpression, variance);
                else
                    throw new IncompatibleTypes(type, castedExpression.getType());
            }
            return new DirectConversion(type, castedExpression, CastType.TO_OPTIONAL, variance);
        }
        if (baseType instanceof FOptional && targetType == FBool.INSTANCE)
            return new DirectConversion(type, castedExpression, CastType.OPTIONAL_TO_BOOL, variance);
        if (baseType instanceof FClass && ((FClass) baseType).getDelegate(targetType) != null)
            return new DirectConversion(type, castedExpression, CastType.DELEGATE, variance); //TODO there are more complex cast paths here

        throw new IncompatibleTypes(type, castedExpression.getType());
    }

    public CastType getCastType() {
        return castType;
    }

    @Override
    public int getCost() {
        switch (castType) {
            case INTEGER_PROMOTION:
                return castType.getCost(((FIntN) getType()).getN() - ((FIntN) getCastedExpression().getType()).getN());
            case FLOAT_PROMOTION:
                return castType.getCost(0);
            case TO_OPTIONAL:
                return castType.getCost(0);
            case OPTIONAL_TO_BOOL:
                return castType.getCost(0);
            case DELEGATE:
                return castType.getCost(32 * ((FClass) getCastedExpression().getType()).getDelegate(getType()).size());
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
                return getCastedExpression().getType() != FBool.INSTANCE;
            case OPTIONAL_TO_BOOL:
                return false;
            case DELEGATE:
                return false;
            default:
                return Utils.cantHappen();
        }
    }
}

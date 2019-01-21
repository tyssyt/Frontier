package tys.frontier.code.expression.cast;

import tys.frontier.code.FClass;
import tys.frontier.code.FType;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FImplicitCast extends FCast {

    public enum CastType {
        INTEGER_PROMOTION(0, 1),
        FLOAT_PROMOTION(32, 0),
        TO_OPTIONAL(32, 0),
        OPTIONAL_TO_BOOL(32, 0), //this is a special case and should never be done as an inner cast
        DELEGATE(0, 32);
        //TODO function cast, co-&contraivariant casts

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

    private FImplicitCast(FType type, FExpression castedExpression) throws IncompatibleTypes {
        super(type, castedExpression);
        castType = getCastType(type, castedExpression.getType());
    }

    public static FImplicitCast create(FType type, FExpression castedExpression) throws IncompatibleTypes {
        return new FImplicitCast(type, castedExpression);
    }

    public static FImplicitCast createTrusted(FType type, FExpression castedExpression) {
        try {
            return create(type, castedExpression);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public static CastType getCastType(FType targetType, FType baseType) throws IncompatibleTypes {
        if (targetType instanceof FIntN && baseType instanceof FIntN &&
                ((FIntN) targetType).getN() > ((FIntN) baseType).getN())
            return CastType.INTEGER_PROMOTION;
        //TODO upwards float cast
        if (targetType instanceof FOptional) {
            FType targetBase = ((FOptional) targetType).getBaseType();
            if (baseType instanceof FOptional) {
                //TODO covariant inner cast
            } else {
                if (baseType == targetBase) {
                    return CastType.TO_OPTIONAL;
                } else {
                    //TODO try to cast inner types (cause optional is covariant) and then wrap that into an toOptional
                }
            }
        }
        if (baseType instanceof FOptional && targetType == FBool.INSTANCE)
            return CastType.OPTIONAL_TO_BOOL;
        if (baseType instanceof FClass && ((FClass) baseType).getDelegate(targetType) != null)
            return CastType.DELEGATE;

        throw new IncompatibleTypes(targetType, baseType);
    }

    public CastType getCastType() {
        return castType;
    }

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
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterImplicitCast(this);
        return visitor.exitImplicitCast(this, getCastedExpression().accept(visitor));
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitImplicitCast(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return getCastedExpression().toString(sb);
    }
}

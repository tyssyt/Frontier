package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FType;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FImplicitCast extends FCast {

    public enum CastType {
        INTEGER_PROMOTION,
        FLOAT_PROMOTION,
        INT_TO_FLOAT,
        TO_OPTIONAL,
        OPTIONAL_TO_BOOL,
        DELEGATE,
    }

    private CastType castType;

    public FImplicitCast(FType type, FExpression castedExpression) throws IncompatibleTypes {
        super(type, castedExpression);
        castType = getCastType(type, castedExpression.getType());
    }

    public static CastType getCastType(FType targetType, FType baseType) throws IncompatibleTypes {
        if (targetType instanceof FIntN && baseType instanceof FIntN &&
                ((FIntN) targetType).getN() > ((FIntN) baseType).getN())
            return CastType.INTEGER_PROMOTION;
        //TODO int to float cast
        //TODO upwards float cast
        if (targetType instanceof FOptional && baseType == ((FOptional) targetType).getBaseType())
            return CastType.TO_OPTIONAL;
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
                return ((FIntN) getType()).getN() - ((FIntN) getCastedExpression().getType()).getN();
            case FLOAT_PROMOTION:
                return 32;
            case INT_TO_FLOAT:
                return 64;
            case TO_OPTIONAL:
                return 32;
            case OPTIONAL_TO_BOOL:
                return 32;
            case DELEGATE:
                return 32* ((FClass) getCastedExpression().getType()).getDelegate(getType()).size();
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

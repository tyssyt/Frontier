package tys.frontier.code.expression.cast;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FExplicitCast extends FCast {

    public enum CastType {
        INTEGER_DEMOTION,
        FLOAT_DEMOTION,
        FLOAT_TO_INT,
        INT_TO_FLOAT,
        REMOVE_OPTIONAL,
    }

    private CastType castType;
    private FType type;

    private FExplicitCast(Position position, FType type, FExpression castedExpression) throws IncompatibleTypes {
        super(position, castedExpression);
        this.type = type;
        castType = getCastType(getType(), getCastedExpression().getType());
    }

    public static FExplicitCast create(Position position, FType type, FExpression castedExpression) throws IncompatibleTypes {
        return new FExplicitCast(position, type, castedExpression);
    }

    public static FExplicitCast createTrusted(Position position, FType type, FExpression castedExpression) {
        try {
            return create(position, type, castedExpression);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    @Override
    public FType getType() {
        return type;
    }

    public CastType getCastType() {
        return castType;
    }

    public static CastType getCastType(FType targetType, FType baseType) throws IncompatibleTypes {
        if (targetType instanceof FIntN && baseType instanceof FIntN &&
                ((FIntN) targetType).getN() < ((FIntN) baseType).getN())
            return CastType.INTEGER_DEMOTION;
        if (targetType instanceof FFloat32 && baseType instanceof FFloat64)
            return CastType.FLOAT_DEMOTION;
        if ((baseType == FFloat32.INSTANCE ||baseType == FFloat64.INSTANCE) && targetType instanceof FIntN)
            return CastType.FLOAT_TO_INT;
        if (baseType instanceof FIntN && (targetType == FFloat32.INSTANCE || targetType == FFloat64.INSTANCE))
            return CastType.INT_TO_FLOAT;
        if (baseType instanceof FOptional && targetType == ((FOptional) baseType).getBaseType())
            return CastType.REMOVE_OPTIONAL;

        throw new IncompatibleTypes(targetType, baseType);
    }

    @Override
    public boolean isNoOpCast() {
        return switch (castType) {
            case INTEGER_DEMOTION -> false;
            case FLOAT_DEMOTION   -> false;
            case FLOAT_TO_INT     -> false;
            case INT_TO_FLOAT     -> false;
            case REMOVE_OPTIONAL  -> getType() != FBool.INSTANCE;
        };
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterExplicitCast(this);
        return visitor.exitExplicitCast(this, getCastedExpression().accept(visitor));
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitExplicitCast(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append('(').append(getType().getIdentifier().name).append(')');
        return getCastedExpression().toString(sb);
    }
}

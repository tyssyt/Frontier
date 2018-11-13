package tys.frontier.code.expression;

import tys.frontier.code.FType;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FExplicitCast extends FCast {

    public enum CastType {
        INTEGER_DEMOTION,
        FLOAT_DEMOTION,
        FLOAT_TO_INT,
        REMOVE_OPTIONAL,
    }

    private CastType castType;

    private FExplicitCast(FType type, FExpression castedExpression) throws IncompatibleTypes {
        super(type, castedExpression);
        castType = getCastType(getType(), getCastedExpression().getType());
    }

    public static FExplicitCast create(FType type, FExpression castedExpression) throws IncompatibleTypes {
        return new FExplicitCast(type, castedExpression);
    }

    public static FExplicitCast createTrusted(FType type, FExpression castedExpression) {
        try {
            return create(type, castedExpression);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public CastType getCastType() {
        return castType;
    }

    public static CastType getCastType(FType targetType, FType baseType) throws IncompatibleTypes {
        if (targetType instanceof FIntN && baseType instanceof FIntN &&
                ((FIntN) targetType).getN() < ((FIntN) baseType).getN())
            return CastType.INTEGER_DEMOTION;
        //TODO float to int
        //TODO downwards float cast
        if (baseType instanceof FOptional && targetType == ((FOptional) baseType).getBaseType())
            return CastType.REMOVE_OPTIONAL;

        throw new IncompatibleTypes(targetType, baseType);
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

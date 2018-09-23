package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class FImplicitCast extends FCast {

    public enum CastType {
        INTEGER_PROMOTION,
        FLOAT_PROMOTION,
        INT_TO_FLOAT,
        DELEGATE
    }

    private CastType castType;

    public FImplicitCast(FClass type, FExpression castedExpression) throws IncompatibleTypes {
        super(type, castedExpression);
        castType = getCastType(type, castedExpression.getType());
    }

    public static CastType getCastType(FClass targetType, FClass baseType) throws IncompatibleTypes {
        if (targetType instanceof FIntN && baseType instanceof FIntN &&
                ((FIntN) targetType).getN() > ((FIntN) baseType).getN())
            return CastType.INTEGER_PROMOTION;
        //TODO int to float cast
        //TODO upwards float cast
        if (baseType.getDelegate(targetType) != null)
            return CastType.DELEGATE;

        throw new IncompatibleTypes(targetType, baseType);
    }

    public CastType getCastType() {
        return castType;
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

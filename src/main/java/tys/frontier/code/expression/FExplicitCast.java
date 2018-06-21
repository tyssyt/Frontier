package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class FExplicitCast extends FCast implements NeedsTypeCheck {

    public enum CastType {
        INTEGER_DEMOTION,
        FLOAT_DEMOTION,
        FLOAT_TO_INT,
        OBJECT_PROMOTION
    }

    private CastType castType;

    public FExplicitCast(FClass type, FExpression castedExpression) {
        super(type, castedExpression);
    }

    public CastType getCastType() {
        return castType;
    }

    public static CastType getCastType(FClass targetType, FClass baseType) throws IncompatibleTypes {
        if (targetType instanceof FIntN && baseType instanceof FIntN &&
                ((FIntN) targetType).getN() < ((FIntN) baseType).getN())
            return CastType.INTEGER_DEMOTION;
        //TODO float to int
        //TODO downwards float cast
        if (targetType.isSubType(baseType))
            return CastType.OBJECT_PROMOTION;

        throw new IncompatibleTypes(targetType, baseType);
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        castType = getCastType(getType(), getCastedExpression().getType());
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

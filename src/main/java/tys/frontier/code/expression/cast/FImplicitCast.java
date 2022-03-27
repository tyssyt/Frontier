package tys.frontier.code.expression.cast;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.Pack;
import tys.frontier.code.expression.UnboundExpression;
import tys.frontier.code.literal.*;
import tys.frontier.code.predefinedClasses.FIntLiteralType;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.typeInference.Constraints;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Utils;

public class FImplicitCast extends FCast {

    private ImplicitTypeCast typeCast;

    private FImplicitCast(Position position, FExpression castedExpression, ImplicitTypeCast typeCast) {
        super(position, castedExpression);
        this.typeCast = typeCast;
    }

    public static FExpression create(Position position, FType targetType, FExpression castedExpression, Variance variance) throws IncompatibleTypes {
        if (castedExpression instanceof FImplicitCast)
            castedExpression = ((FImplicitCast) castedExpression).getCastedExpression();

        if (castedExpression instanceof UnboundExpression)
            return ((UnboundExpression) castedExpression).bind(targetType);

        FType baseType = castedExpression.getType();
        if (baseType == targetType)
            return castedExpression;

        if (castedExpression instanceof FLiteralExpression) {
            FLiteral literal = ((FLiteralExpression) castedExpression).getLiteral();
            if (literal == FNull.UNTYPED && FOptional.canBeTreatedAsOptional(targetType)) {
                return new FLiteralExpression(position, new FNull(targetType));
            }
            if (literal instanceof FIntNLiteral && targetType instanceof FIntN) {
                FLiteralExpression intLiteral = handleIntLiteral(castedExpression.getPosition(), (FIntNLiteral) literal, (FIntN) targetType);
                if (intLiteral != null)
                    return intLiteral;
            }
            if (literal instanceof FIntNLiteral && targetType instanceof FOptional && ((FOptional) targetType).getBaseType() instanceof FIntN) {
                FLiteralExpression intLiteral = handleIntLiteral(castedExpression.getPosition(), (FIntNLiteral) literal, (FIntN) ((FOptional) targetType).getBaseType());
                if (intLiteral != null)
                    castedExpression = intLiteral;
            }
            if (literal instanceof FCharLiteral && targetType == FStringLiteral.TYPE) {
                return new FLiteralExpression(castedExpression.getPosition(), new FStringLiteral("" + ((FCharLiteral) literal).value));
            }
            if (literal instanceof FCharLiteral && targetType == FOptional.from(FStringLiteral.TYPE)) {
                castedExpression = new FLiteralExpression(castedExpression.getPosition(), new FStringLiteral("" + ((FCharLiteral) literal).value));
            }
        }

        if (castedExpression instanceof Pack && targetType instanceof FTuple) {
            Pack pack = (Pack) castedExpression;
            FTuple tuple = (FTuple) targetType;

            if (pack.getExpressions().size() == tuple.arity()) // distribute the cast over all expressions in the pack
                return new Pack(pack.getPosition(), pack.getExpressions(), tuple.getTypes());
        }

        Constraints constraints = new Constraints();
        FImplicitCast res = new FImplicitCast(position, castedExpression, ImplicitTypeCast.create(baseType, targetType, variance, constraints));

        for (ImplicitCastable constraint : constraints.values())
            constraint.setOrigin(res);

        try {
            constraints.checkAll();
        } catch (UnfulfillableConstraints e) {
            throw new IncompatibleTypes(position, targetType, castedExpression.getType());
        }
        return res;
    }

    public static FExpression createTrusted(Position position, FType type, FExpression castedExpression, Variance variance) {
        try {
            return create(position, type, castedExpression, variance);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    private static FLiteralExpression handleIntLiteral(Position position, FIntNLiteral intLiteral, FIntN targetInt) {
        if (intLiteral.getType() instanceof FIntLiteralType)
            return new FLiteralExpression(position, new FIntNLiteral(intLiteral.value, targetInt));
        if (intLiteral.getType() instanceof FIntN && ((FIntN) intLiteral.getType()).getN() < targetInt.getN())
            return new FLiteralExpression(position, new FIntNLiteral(intLiteral.value, targetInt));
        return null;
    }

    public ImplicitTypeCast getTypeCast() {
        return typeCast;
    }

    @Override
    public boolean isNoOpCast() {
        return typeCast.isNoOpCast();
    }

    @Override
    public FType getType() {
        return typeCast.getTarget();
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

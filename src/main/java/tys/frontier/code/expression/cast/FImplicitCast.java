package tys.frontier.code.expression.cast;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.MultimapBuilder;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.Map;

public class FImplicitCast extends FCast { //TODO consider removing all the forwared methods and haveing the caller of the just use the typeCast getter

    private ImplicitTypeCast typeCast;

    private FImplicitCast(FExpression castedExpression, ImplicitTypeCast typeCast) {
        super(castedExpression);
        this.typeCast = typeCast;
    }

    public static FExpression create(FType targetType, FExpression castedExpression, Variance variance) throws IncompatibleTypes {
        ListMultimap<FTypeVariable, TypeConstraint> constraints = MultimapBuilder.hashKeys().arrayListValues().build();
        if (castedExpression instanceof FImplicitCast)
            castedExpression = ((FImplicitCast) castedExpression).getCastedExpression();
        FExpression res = create(targetType, castedExpression, variance, constraints);
        for (Map.Entry<FTypeVariable, TypeConstraint> entry : constraints.entries()) {
            if (!entry.getKey().tryAddConstraint(entry.getValue()))
                throw new IncompatibleTypes(targetType, castedExpression.getType());
        }
        return res;
    }

    public static FExpression create(FType targetType, FExpression castedExpression, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        FType baseType = castedExpression.getType();
        if (baseType == targetType)
            return castedExpression;
        if (castedExpression instanceof FLiteralExpression) {
            FLiteral literal = ((FLiteralExpression) castedExpression).getLiteral();
            if (literal == FNull.UNTYPED && targetType instanceof FOptional) {
                return new FLiteralExpression(new FNull((FOptional) targetType));
            }
        }
        FImplicitCast res = new FImplicitCast(castedExpression, ImplicitTypeCast.create(baseType, targetType, variance, constraints));
        for (TypeConstraint constraint : constraints.values()) {
            constraint.setOrigin(res);
        }
        return res;
    }

    public static FExpression createTrusted(FType type, FExpression castedExpression, Variance variance) {
        try {
            return create(type, castedExpression, variance);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public static FExpression createTrusted(FType type, FExpression castedExpression, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) {
        try {
            return create(type, castedExpression, variance, constraints);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
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

    public int getCost() {
        return typeCast.getCost();
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

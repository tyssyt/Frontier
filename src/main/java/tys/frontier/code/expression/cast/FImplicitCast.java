package tys.frontier.code.expression.cast;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.Map;

import static tys.frontier.code.typeInference.Variance.Covariant;

public class FImplicitCast extends FCast { //TODO consider removing all the forwared methods and haveing the caller of the just use the typeCast getter

    private ImplicitTypeCast typeCast;

    private FImplicitCast(FExpression castedExpression, ImplicitTypeCast typeCast) {
        super(castedExpression);
        this.typeCast = typeCast;
    }

    public static FExpression create(FType targetType, FExpression castedExpression) throws IncompatibleTypes {
        ArrayListMultimap<FTypeVariable, TypeConstraint> constraints = ArrayListMultimap.create();
        FExpression res = create(targetType, castedExpression, Covariant, constraints);
        for (Map.Entry<FTypeVariable, TypeConstraint> entry : constraints.entries()) {
            entry.getKey().addConstraint(entry.getValue());
        }
        return res;
    }

    public static FExpression create(FType targetType, FExpression castedExpression, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        FType baseType = castedExpression.getType();
        if (baseType == targetType)
            return castedExpression;
        return new FImplicitCast(castedExpression, ImplicitTypeCast.create(baseType, targetType, variance, constraints));
    }

    public static FExpression createTrusted(FType type, FExpression castedExpression) {
        try {
            return create(type, castedExpression);
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

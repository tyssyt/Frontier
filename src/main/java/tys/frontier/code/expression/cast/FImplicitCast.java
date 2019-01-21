package tys.frontier.code.expression.cast;

import tys.frontier.code.FInstantiatedClass;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import static tys.frontier.code.typeInference.Variance.Contravariant;
import static tys.frontier.code.typeInference.Variance.Covariant;

public abstract class FImplicitCast extends FCast {

    protected Variance variance;

    protected FImplicitCast(FType type, FExpression castedExpression, Variance variance) {
        super(type, castedExpression);
        this.variance = variance;
    }

    public static FImplicitCast create(FType targetType, FExpression castedExpression) throws IncompatibleTypes {
        return create(targetType, castedExpression, Contravariant);
    }

    public static FImplicitCast create(FType targetType, FExpression castedExpression, Variance variance) throws IncompatibleTypes {
        assert variance == Covariant || variance == Contravariant; //why else would we cast?
        FType baseType = castedExpression.getType();
        if (targetType instanceof FTypeVariable || baseType instanceof FTypeVariable)
            throw new IncompatibleTypes(targetType, baseType); //TODO this actually always succeeds, but we need more knowledge of vars and to create and return contraints
        if (targetType instanceof FInstantiatedClass && baseType instanceof FInstantiatedClass && ((FInstantiatedClass) targetType).getBaseClass() == ((FInstantiatedClass) baseType).getBaseClass())
            return TypeParameterCast.createTTTTT(((FInstantiatedClass) targetType), castedExpression, variance); //TODO what if one of them is the base class, I think thats in theory possible
        if (targetType instanceof FOptional && baseType instanceof FOptional)
            return TypeParameterCast.createTTTTT(((FOptional) targetType), castedExpression, variance); //TODO optional will be made generic some day
        if (targetType instanceof FFunctionType && baseType instanceof FFunctionType)
            return TypeParameterCast.createTTTTT(((FFunctionType) targetType), castedExpression, variance); //TODO function types will be made generic some day
        return DirectConversion.create(targetType, castedExpression, variance);
    }

    public static FImplicitCast createTrusted(FType type, FExpression castedExpression) {
        return createTrusted(type, castedExpression, Covariant);
    }

    public static FImplicitCast createTrusted(FType type, FExpression castedExpression, Variance variance) {
        try {
            return create(type, castedExpression, variance);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public abstract int getCost();

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

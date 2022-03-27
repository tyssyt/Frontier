package tys.frontier.code.expression.cast;

import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.Constraints;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.Optional;

public abstract class ImplicitTypeCast {

    protected FType base;
    protected FType target;
    protected Variance variance;

    protected ImplicitTypeCast(FType base, FType target, Variance variance) {
        this.base = base;
        this.target = target;
        this.variance = variance;
    }

    public static ImplicitTypeCast create(FType baseType, FType targetType, Variance variance, Constraints constraints) throws IncompatibleTypes {
        assert baseType != targetType;

        if (baseType == FNull.NULL_TYPE) {
            if (FOptional.canBeTreatedAsOptional(targetType))
                return new LiteralCast(baseType, targetType, variance);
            else
                throw new IncompatibleTypes(targetType, baseType); //can't assign null to a non-optional type
        }

        if (targetType == FNull.NULL_TYPE)
            return Utils.cantHappen();

        if (baseType instanceof FIntLiteralType && targetType instanceof FIntN) {
            return new LiteralCast(baseType, targetType, variance);
        }
        if (baseType instanceof FIntN && targetType instanceof FIntLiteralType) {
            return new LiteralCast(baseType, targetType, variance);
        }

        //first check if either base or targetType is a TypeVariable and do a type variable cast
        if (baseType instanceof FTypeVariable || targetType instanceof FTypeVariable) {
            return TypeVariableCast.createTVC(baseType, targetType, variance, constraints);
        }

        //special case where we cast T? to void (or vice versa)
        if (baseType == FTuple.VOID || targetType == FTuple.VOID) {
            if (baseType instanceof FOptional && ((FOptional) baseType).getBaseType() instanceof FTypeVariable) {
                FTypeVariable typeVariable = (FTypeVariable) ((FOptional) baseType).getBaseType();
                return TypeVariableCast.createTVC(typeVariable, FTuple.VOID, variance, constraints);
            }
            if (targetType instanceof FOptional && ((FOptional) targetType).getBaseType() instanceof FTypeVariable) {
                FTypeVariable typeVariable = (FTypeVariable) ((FOptional) targetType).getBaseType();
                return TypeVariableCast.createTVC(FTuple.VOID, typeVariable, variance, constraints);
            }
        }

        if (targetType instanceof FInstantiatedClass && baseType instanceof FInstantiatedClass && ((FInstantiatedClass) targetType).getProxy() == ((FInstantiatedClass) baseType).getProxy())
            return TypeParameterCast.createTPC((FInstantiatedClass) baseType, (FInstantiatedClass) targetType, variance, constraints); //TODO what if one of them is the base class, I think thats in theory possible
        if (targetType instanceof FOptional && baseType instanceof FOptional)
            return TypeParameterCast.createTPC((FOptional) baseType, (FOptional) targetType, variance, constraints); //TODO optional will be made generic some day
        if (targetType instanceof FFunctionType && baseType instanceof FFunctionType)
            return TypeParameterCast.createTPC((FFunctionType) baseType, (FFunctionType) targetType, variance, constraints); //TODO function types will be made generic some day
        if (targetType instanceof FTuple && baseType instanceof FTuple) {
            //weird special cases because Optionals of Tuples are weird
            Optional<TypeConversion> tupleToOptional = TypeConversion.tryTupleToOptional((FTuple) baseType, (FTuple) targetType, variance);
            if (tupleToOptional.isPresent())
                return tupleToOptional.get();
            return TypeParameterCast.createTPC((FTuple) baseType, (FTuple) targetType, variance, constraints);
        }
        return TypeConversion.createTC((FClass) baseType, (FClass) targetType, variance, constraints);
    }




    public FType getBase() {
        return base;
    }

    public FType getTarget() {
        return target;
    }

    public Variance getVariance() {
        return variance;
    }

    public abstract long getCost();

    public abstract boolean isNoOpCast();
}

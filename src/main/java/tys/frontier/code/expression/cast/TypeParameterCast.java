package tys.frontier.code.expression.cast;

import com.google.common.collect.Multimap;
import tys.frontier.code.*;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.List;

import static tys.frontier.code.typeInference.Variance.Contravariant;
import static tys.frontier.code.typeInference.Variance.Covariant;

public class TypeParameterCast extends ImplicitTypeCast {

    private ImplicitTypeCast[] casts;

    private TypeParameterCast(FInstantiatedClass base, FInstantiatedClass target, Variance variance, ImplicitTypeCast[] casts) {
        super(base, target, variance);
        this.casts = casts;
    }

    //TODO remove once optionals are generic
    public static TypeParameterCast createTPC(FOptional baseType, FOptional targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        return Utils.NYI("implicit inner cast Optional");
    }

    //TODO remove once functionType is generic
    public static TypeParameterCast createTPC(FFunctionType baseType, FFunctionType targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        return Utils.NYI("implicit inner cast FunctionType");
    }

    public static TypeParameterCast createTPC(FInstantiatedClass baseType, FInstantiatedClass targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        assert variance == Covariant || variance == Contravariant;
        assert baseType != targetType;
        FClass nonParameterized = baseType.getBaseClass();
        assert  nonParameterized == targetType.getBaseClass();
        TypeInstantiation baseInst = baseType.getTypeInstantiation();
        TypeInstantiation targetInst = targetType.getTypeInstantiation();

        //get BaseClass Parameter list
        List<FTypeVariable> parameters = nonParameterized.getParametersList();
        ImplicitTypeCast[] casts = new ImplicitTypeCast[parameters.size()];

        //for each baseClass parameter, instantiated with the instantiations from both sides
        for (int i = 0; i < parameters.size(); i++) {
            FTypeVariable param = parameters.get(i);
            FType b = baseInst.getType(param);
            FType t = targetInst.getType(param);
            assert param != b || param != t;

            if (b==t)
                continue;

            //try to implicit cast from one side to the other (using the variance of the base parameter)
            Variance paramVar = variance.then(nonParameterized.getParameterVariance(param));
            casts[i] = ImplicitTypeCast.create(b, t, paramVar, constraints); //fails just propagate TODO at some point catch the fail and improve the error message
        }
        return new TypeParameterCast(baseType, targetType, variance, casts);
    }

    @Override
    public FInstantiatedClass getBase() {
        return (FInstantiatedClass) base;
    }

    @Override
    public FInstantiatedClass getTarget() {
        return (FInstantiatedClass) target;
    }

    @Override
    public int getCost() {
        int cost = 0;
        for (ImplicitTypeCast cast : casts) {
            if (cast != null)
                cost += cast.getCost();
        }
        return cost;
    }

    @Override
    public boolean isNoOpCast() {
        for (ImplicitTypeCast cast : casts) {
            if (cast != null && !cast.isNoOpCast())
                return false;
        }
        return true;
    }
}

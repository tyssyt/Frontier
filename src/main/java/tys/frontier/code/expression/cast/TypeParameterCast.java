package tys.frontier.code.expression.cast;

import com.google.common.collect.Multimap;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

import java.util.List;

import static tys.frontier.code.typeInference.Variance.*;

public class TypeParameterCast extends ImplicitTypeCast {

    private ImplicitTypeCast[] casts;

    private TypeParameterCast(FInstantiatedClass base, FInstantiatedClass target, Variance variance, ImplicitTypeCast[] casts) {
        super(base, target, variance);
        this.casts = casts;
    }

    private TypeParameterCast(FOptional base, FOptional target, Variance variance, ImplicitTypeCast[] casts) {
        super(base, target, variance);
        this.casts = casts;
    }

    private TypeParameterCast(FArray base, FArray target, Variance variance, ImplicitTypeCast[] casts) {
        super(base, target, variance);
        this.casts = casts;
    }

    private TypeParameterCast(FFunctionType base, FFunctionType target, Variance variance, ImplicitTypeCast[] casts) {
        super(base, target, variance);
        this.casts = casts;
    }

    private TypeParameterCast(FTuple base, FTuple target, Variance variance, ImplicitTypeCast[] casts) {
        super(base, target, variance);
        this.casts = casts;
    }

    //TODO remove once optionals are generic
    public static TypeParameterCast createTPC(FOptional baseType, FOptional targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        assert variance == Covariant || variance == Contravariant;
        assert baseType != targetType;

        ImplicitTypeCast[] casts = new ImplicitTypeCast[] {
                ImplicitTypeCast.create(baseType.getBaseType(), targetType.getBaseType(), variance, constraints)
        };

        return new TypeParameterCast(baseType, targetType, variance, casts);
    }

    //TODO remove once arrays are generic
    public static TypeParameterCast createTPC(FArray baseType, FArray targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        assert variance == Covariant || variance == Contravariant;
        assert baseType != targetType;

        ImplicitTypeCast[] casts = new ImplicitTypeCast[] {
                ImplicitTypeCast.create(baseType.getBaseType(), targetType.getBaseType(), Invariant, constraints) //TODO is invariant right?
        };

        return new TypeParameterCast(baseType, targetType, variance, casts);
    }

    //TODO remove once functionType is generic
    public static TypeParameterCast createTPC(FFunctionType baseType, FFunctionType targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        assert variance == Covariant || variance == Contravariant;
        assert baseType != targetType;

        ImplicitTypeCast in = null;
        if (baseType.getIn() != targetType.getIn())
            in = ImplicitTypeCast.create(baseType.getIn(), targetType.getIn(), variance.then(Contravariant), constraints); //Parameter Types are Contravariant
        ImplicitTypeCast out = null;
        if (baseType.getOut() != targetType.getOut())
            out = ImplicitTypeCast.create(baseType.getOut(), targetType.getOut(), variance, constraints); //Return Type is Covariant

        return new TypeParameterCast(baseType, targetType, variance, new ImplicitTypeCast[] {in, out});
    }

    public static TypeParameterCast createTPC(FInstantiatedClass baseType, FInstantiatedClass targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        assert variance == Covariant || variance == Contravariant;
        assert baseType != targetType;
        FClass nonParameterized = baseType.getProxy();
        assert  nonParameterized == targetType.getProxy();
        TypeInstantiation baseInst = baseType.getTypeInstantiation();
        TypeInstantiation targetInst = targetType.getTypeInstantiation();

        //get BaseClass Parameter list
        //noinspection unchecked
        List<FTypeVariable> parameters = ((List<FTypeVariable>) nonParameterized.getParametersList());
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

    public static TypeParameterCast createTPC(FTuple baseType, FTuple targetType, Variance variance, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleTypes {
        assert variance == Covariant || variance == Contravariant;
        assert baseType != targetType;

        List<FType> baseTypes = baseType.getTypes();
        List<FType> targetTypes = targetType.getTypes();

        if (baseTypes.size() != targetTypes.size())
            throw new IncompatibleTypes(targetType, baseType);

        ImplicitTypeCast[] casts = new ImplicitTypeCast[baseTypes.size()];

        for (int i = 0; i < baseTypes.size(); i++) {
            FType b = baseTypes.get(i);
            FType t = targetTypes.get(i);

            if (b==t)
                continue;

            //TODO think about variance
            casts[i] = ImplicitTypeCast.create(b, t, variance, constraints); //fails just propagate TODO at some point catch the fail and improve the error message
        }

        return new TypeParameterCast(baseType, targetType, variance, casts);
    }

    public ImplicitTypeCast[] getCasts() {
        return casts;
    }

    @Override
    public FClass getBase() {
        return (FClass) base;
    }

    @Override
    public FClass getTarget() {
        return (FClass) target;
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

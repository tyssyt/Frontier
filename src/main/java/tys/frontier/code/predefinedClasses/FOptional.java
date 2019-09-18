package tys.frontier.code.predefinedClasses;

import com.google.common.collect.*;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FOptionalIdentifier;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;

public class FOptional extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FType, FOptional> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType baseType;
    private BiMap<FFunction, FFunction> shimMap = HashBiMap.create();

    private FOptional(FType baseType) {
        super(new FOptionalIdentifier(baseType.getIdentifier()));
        assert !(baseType instanceof FOptional);
        assert baseType != FTuple.VOID;
        this.baseType = baseType;
        addDefaultFunctions();
    }

    public BiMap<FFunction, FFunction> getShimMap() {
        return shimMap;
    }

    public FFunction getOriginalFunction(FFunction function) {
        return shimMap.inverse().get(function);
    }

    @Override
    public long concreteness() { //TODO once optionals use generics this is no longer necessary
        long res = baseType.concreteness();
        if (res == Long.MAX_VALUE) //avoid overflow
            return Long.MAX_VALUE;
        return res+1;
    }

    @Override
    public boolean canImplicitlyCast() {  //TODO once optionals use generics this is no longer necessary
        return true;
    }

    @Override
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType, TypeInstantiation typeInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints) throws FunctionNotFound {
        if (positionalArgs.size() > 0 && positionalArgs.get(0) == this) {
            positionalArgs = new ArrayList<>(positionalArgs); //copy to not modify the original list
            positionalArgs.set(0, baseType);
        }
        FFunction base = baseType.resolveFunction(identifier, positionalArgs, keywordArgs, returnType, typeInstantiation, constraints);
        return shimMap.computeIfAbsent(base, this::createShim);
    }

    private FFunction createShim(FFunction original) {
        FType returnType = original.getType() == FTuple.VOID ? FTuple.VOID : FOptional.fromFlatten(original.getType());
        ImmutableList<FParameter> params = original.getParams();
        if (original.isInstance()) {
            ImmutableList.Builder<FParameter> builder = ImmutableList.builder();
            builder.add(FParameter.create(params.get(0).getIdentifier(), this, false));
            builder.addAll(params.subList(1, params.size()));
            params = builder.build();
        }

        return new FBaseFunction(original.getIdentifier(), this, original.getVisibility(), false,
                returnType, ImmutableList.copyOf(params)) {
            {predefined = true;}
        };
    }

    public static FBaseClass from(FType baseClass) {
        if (baseClass == FTuple.VOID)
            return FTuple.VOID;
        if (baseClass instanceof FTuple)
            return Utils.NYI("optional of Tuple"); //TODO
        return existing.computeIfAbsent(baseClass, FOptional::new);
    }

    public static FBaseClass fromFlatten(FType baseClass) {
        if (baseClass instanceof FOptional)
            return (FOptional) baseClass;
        else
            return from(baseClass);
    }

    public FType getBaseType() {
        return baseType;
    }
}

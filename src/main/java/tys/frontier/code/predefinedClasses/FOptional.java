package tys.frontier.code.predefinedClasses;

import com.google.common.collect.*;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FOptionalIdentifier;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentMap;

import static java.util.Collections.emptyMap;

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
        addFunctionTrusted(UnaryOperator.NOT.createPredefined(this, FBool.INSTANCE));
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
    public FunctionResolver.Result softResolveFunction(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve) throws FunctionNotFound {
        if (positionalArgs.size() > 0 && positionalArgs.get(0) == this) {
            positionalArgs = new ArrayList<>(positionalArgs); //copy to not modify the original list
            positionalArgs.set(0, baseType);
        }
        if (identifier.equals(UnaryOperator.NOT.identifier)) {
            return FunctionResolver.resolve(identifier, positionalArgs, keywordArgs, returnType, getFunctions(lhsResolve).get(identifier));
        }
        FunctionResolver.Result res = baseType.softResolveFunction(identifier, positionalArgs, keywordArgs, returnType, lhsResolve);
        FFunction shim = shimMap.computeIfAbsent(res.getFunction(), this::createShim);
        res.signature = lhsResolve ? shim.getLhsSignature() : shim.getSignature();
        return res;
    }

    private FFunction createShim(FFunction original) {
        FType returnType = FOptional.fromFlatten(original.getType());
        Signature sig = original.getLhsSignature() == null ? original.getSignature() : original.getLhsSignature();

        ImmutableList<FParameter> params = sig.getParameters();
        if (original.isInstance()) {
            ImmutableList.Builder<FParameter> builder = ImmutableList.builder();
            builder.add(FParameter.create(params.get(0).getIdentifier(), this, false));
            builder.addAll(params.subList(1, params.size()));
            params = builder.build();
        }

        return new FBaseFunction(original.getIdentifier(), this, original.getVisibility(), false,
                returnType, params, sig.getAssignees(), emptyMap()) {
            {predefined = true;}
        };
    }

    public static FBaseClass from(FType baseClass) {
        if (baseClass instanceof FTuple) {
            if (baseClass == FTuple.VOID)
                return FTuple.VOID;
            //the optional of a Tuple is a Tuple of Optionals
            FTuple tuple = (FTuple) baseClass;
            List<FType> optionalBases = new ArrayList<>(tuple.arity());
            for (FType base : tuple.getTypes()) {
                optionalBases.add(fromFlatten(base));
            }
            return (FTuple) FTuple.from(optionalBases);
        }
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

    public static boolean canBeTreatedAsOptional(FType fType) {
        return fType instanceof FOptional ||
              (fType instanceof FTuple && Iterables.all(((FTuple) fType).getTypes(), FOptional::canBeTreatedAsOptional));
    }
}

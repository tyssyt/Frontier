package tys.frontier.code.namespace;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ListMultimap;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FOptionalIdentifier;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;

import java.util.ArrayList;
import java.util.List;

public class OptionalNamespace extends DefaultNamespace {

    private Namespace baseNamespace;
    private BiMap<FFunction, FFunction> shimMap = HashBiMap.create();

    private OptionalNamespace(Location location, FOptional fOptional, FIdentifier identifier, FVisibilityModifier visibilityModifier, Namespace baseNamespace) {
        super(location, identifier, visibilityModifier, null, fOptional);
        this.baseNamespace = baseNamespace;
    }

    public static OptionalNamespace create(FOptional fOptional) {
        Namespace baseNamespace = fOptional.getBaseType().getNamespace();
        FOptionalIdentifier identifier = new FOptionalIdentifier(baseNamespace.getIdentifier());
        if (baseNamespace instanceof DefaultNamespace)
            return new OptionalNamespace(baseNamespace.getLocation(), fOptional, identifier, ((DefaultNamespace) baseNamespace).getVisibility(), baseNamespace);
        else
            return new OptionalNamespace(null, fOptional, identifier, FVisibilityModifier.EXPORT, baseNamespace); //TODO what visibility for non Default namespaces?
    }

    @Override
    public FOptional getType() {
        return (FOptional) super.getType();
    }

    public BiMap<FFunction, FFunction> getShimMap() {
        return shimMap;
    }

    public FFunction getOriginalFunction(FFunction function) {
        return shimMap.inverse().get(function);
    }

    @Override
    public FunctionResolver.Result softResolveFunction(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve) throws FunctionNotFound {
        if (positionalArgs.size() > 0 && positionalArgs.get(0) == getType()) {
            positionalArgs = new ArrayList<>(positionalArgs); //copy to not modify the original list
            positionalArgs.set(0, getType().getBaseType());
        }
        if (FOptional.isOriginalOptionalIdentifier(identifier)) {
            return FunctionResolver.resolve(identifier, positionalArgs, keywordArgs, returnType, this, lhsResolve);
        }
        FunctionResolver.Result res = baseNamespace.softResolveFunction(identifier, positionalArgs, keywordArgs, returnType, lhsResolve);
        FFunction shim = shimMap.computeIfAbsent(res.getFunction(), this::createShim);
        res.signature = lhsResolve ? shim.getLhsSignature() : shim.getSignature();
        return res;
    }

    private FFunction createShim(FFunction original) {
        FunctionBuilder builder = new FunctionBuilder(original.getIdentifier(), this).setPredefined(true);
        builder.setVisibility(original.getVisibility());
        builder.setReturnType(FOptional.fromFlatten(original.getType()));

        Signature sig = original.getLhsSignature() == null ? original.getSignature() : original.getLhsSignature();
        builder.setAssignees(sig.getAssignees());

        ImmutableList<FParameter> params = sig.getParameters();
        if (original.isInstance()) {
            ImmutableList.Builder<FParameter> b = ImmutableList.builder();
            b.add(FParameter.create(params.get(0).getPosition(), params.get(0).getIdentifier(), getType(), false));
            b.addAll(params.subList(1, params.size()));
            params = b.build();
        }
        return builder.setLocation(original.getLocation()).setParams(params).build();
    }
}

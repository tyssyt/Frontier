package tys.frontier.code.predefinedClasses;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.FFunction;
import tys.frontier.code.FParameter;
import tys.frontier.code.FType;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FExplicitCast;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FOptionalIdentifier;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentMap;

public class FOptional extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FType, FOptional> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType baseType;
    private BiMap<FFunction, FFunction> shimMap = HashBiMap.create();

    private FOptional(FType baseType) {
        super(new FOptionalIdentifier(baseType.getIdentifier()));
        assert !(baseType instanceof FOptional);
        assert baseType != FVoid.INSTANCE;
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
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        if (arguments.size() > 0 && arguments.get(0).getType() == this) {
            arguments = new ArrayList<>(arguments); //copy to not modify the original list
            //TODO if literals get fixed, resolving can be based on types again and the following can be done by just replacing a type
            arguments.set(0, FExplicitCast.createTrusted(baseType, arguments.get(0)));
        }
        FFunction base = baseType.resolveFunction(identifier, arguments, typeInstantiation);
        return shimMap.computeIfAbsent(base, this::createShim);
    }

    private FFunction createShim(FFunction original) {
        FType returnType = original.getType() == FVoid.INSTANCE ? FVoid.INSTANCE : FOptional.fromFlatten(original.getType());
        ImmutableList<FParameter> params = original.getParams();
        if (original.isInstance()) {
            ImmutableList.Builder<FParameter> builder = ImmutableList.builder();
            builder.add(FParameter.create(params.get(0).getIdentifier(), this, false));
            builder.addAll(params.subList(1, params.size()));
            params = builder.build();
        }

        return new FFunction(original.getIdentifier(), this, original.getVisibility(), false,
                returnType, ImmutableList.copyOf(params)) {
            {predefined = true;}
            @Override
            public boolean addCall(FFunctionCall call) {
                original.addCall(call);
                return super.addCall(call);
            }
        };
    }

    public static FOptional from(FType baseClass) {
        return existing.computeIfAbsent(baseClass, FOptional::new);
    }

    public static FOptional fromFlatten(FType baseClass) {
        if (baseClass instanceof FOptional)
            return (FOptional) baseClass;
        else
            return from(baseClass);
    }

    public FType getBaseType() {
        return baseType;
    }
}

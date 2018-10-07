package tys.frontier.code.predefinedClasses;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FOptionalIdentifier;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.IntIntPair;
import tys.frontier.util.Pair;

import java.util.List;
import java.util.concurrent.ConcurrentMap;

public class FOptional extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FClass, FOptional> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FClass baseType;
    private BiMap<FFunction, FFunction> shimMap = HashBiMap.create();

    private FOptional(FClass baseType) {
        super(new FOptionalIdentifier(baseType.getIdentifier()));
        assert !(baseType instanceof FOptional);
        assert baseType != FVoid.INSTANCE;
        this.baseType = baseType;
        addDefaultFunctions();
    }

    public BiMap<FFunction, FFunction> getShimMap() {
        return shimMap;
    }

    @Override
    public Pair<FFunction, IntIntPair> resolveInstanceFunction(FFunctionIdentifier identifier, List<FExpression> arguments) throws FunctionNotFound {
        Pair<FFunction, IntIntPair> res = baseType.resolveInstanceFunction(identifier, arguments);
        res.a = shimMap.computeIfAbsent(res.a, this::createShim);
        return res;
    }

    private FFunction createShim(FFunction original) {
        FClass returnType = original.getType() == FVoid.INSTANCE ? FVoid.INSTANCE : FOptional.fromFlatten(original.getType());
        return new FFunction(original.getIdentifier(), this, original.getVisibility(), true, false,
                returnType, ImmutableList.copyOf(original.getParams())) {
            {predefined = true;}
            @Override
            public boolean addCall(FFunctionCall call) {
                original.addCall(call);
                return super.addCall(call);
            }
        };
    }

    public static FOptional from(FClass baseClass) {
        return existing.computeIfAbsent(baseClass, p -> new FOptional(baseClass));
    }

    public static FOptional fromFlatten(FClass baseClass) {
        if (baseClass instanceof FOptional)
            return (FOptional) baseClass;
        return existing.computeIfAbsent(baseClass, p -> new FOptional(baseClass));
    }

    public FClass getBaseType() {
        return baseType;
    }
}
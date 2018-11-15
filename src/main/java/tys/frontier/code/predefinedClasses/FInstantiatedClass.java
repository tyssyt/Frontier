package tys.frontier.code.predefinedClasses;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.*;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.util.IntIntPair;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;

public class FInstantiatedClass extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<Pair<FClass, TypeInstantiation>, FInstantiatedClass> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FClass baseClass;
    private TypeInstantiation typeInstantiation;
    private BiMap<FFunction, FFunction> shimMap = HashBiMap.create();

    private FInstantiatedClass(FClass baseClass, TypeInstantiation typeInstantiation) {
        super(baseClass.getIdentifier()); //TODO what identifier
        assert !(baseClass instanceof FInstantiatedClass);
        this.baseClass = baseClass;
        this.typeInstantiation = typeInstantiation;
    }

    public FClass getBaseClass() {
        return baseClass;
    }

    public FFunction getInstantiatedFunction(FFunction function) {
        return shimMap.computeIfAbsent(function, this::createShim);
    }

    public FFunction getOriginalFunction(FFunction function) {
        return shimMap.inverse().get(function);
    }

    public TypeInstantiation getTypeInstantiation() {
        return typeInstantiation;
    }

    @Override
    public Pair<FFunction, IntIntPair> resolveInstanceFunction(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        Pair<FFunction, IntIntPair> res = baseClass.resolveInstanceFunction(identifier, arguments, typeInstantiation.then(this.typeInstantiation));
        res.a = getInstantiatedFunction(res.a);
        return res;
    }

    @Override
    public Pair<FFunction, IntIntPair> resolveStaticFunction(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        Pair<FFunction, IntIntPair> res = baseClass.resolveStaticFunction(identifier, arguments, typeInstantiation.then(this.typeInstantiation));
        res.a = getInstantiatedFunction(res.a);
        return res;
    }

    private FFunction createShim(FFunction original) {
        FType returnType = typeInstantiation.getType(original.getType());
        ImmutableList.Builder<FParameter> params = ImmutableList.builder();
        for (FParameter p : original.getParams()) {
            FType pType = typeInstantiation.getType(p.getType());
            params.add(new FParameter(p.getIdentifier(), pType, p.getDefaultValue().orElse(null)));
        }
        return new FFunction(original.getIdentifier(), this, original.getVisibility(), true, original.isStatic(),
                returnType, params.build()) {
            {predefined = true;}
            @Override
            public boolean addCall(FFunctionCall call) {
                original.addCall(call);
                return super.addCall(call);
            }
        };
    }

    public static FClass from(FClass baseClass, List<FType> types) throws WrongNumberOfTypeArguments {
        if (baseClass.getParameters().size() != types.size()) {
            throw new WrongNumberOfTypeArguments(baseClass, types);
        }
        if (types.size() == 0)
            return baseClass;
        Map<FTypeVariable, FType> typeMap = new HashMap<>();
        for (int i = 0; i < baseClass.getParametersList().size(); i++) {
            if (baseClass.getParametersList().get(i) != types.get(i))
                typeMap.put(baseClass.getParametersList().get(i), types.get(i));
        }
        return from(baseClass, TypeInstantiation.create(typeMap));
    }

    public static FClass from(FClass baseClass, TypeInstantiation typeInstantiation) {
        if (baseClass instanceof FInstantiatedClass)
            return Utils.NYI("specifying within an instantiated class");
        TypeInstantiation intersected = typeInstantiation.intersect(baseClass.getParametersList());
        if (intersected.isEmpty())
            return baseClass;
        return existing.computeIfAbsent(new Pair<>(baseClass, typeInstantiation), p -> new FInstantiatedClass(baseClass, intersected));
    }
}

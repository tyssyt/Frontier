package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;

public class ClassInstantiationFunction extends ForwardingFunction {

    private FBlock newBody;
    private FType newReturnType;
    private ImmutableList<FParameter> newParams;
    private Signature newSignature;
    private FInstantiatedClass newMemberOf;

    private Map<TypeInstantiation, FInstantiatedFunction> newInstantiations;

    private boolean baked = false;

    private ClassInstantiationFunction(FFunction base, FInstantiatedClass memberOf) {
        super(base);
        TypeInstantiation typeInstantiation = memberOf.getTypeInstantiation();
        assert base.getMemberOf() == memberOf.getProxy();
        assert !(base instanceof ClassInstantiationFunction);
        assert typeInstantiation.intersect(base.getParametersList()).isEmpty();
        newReturnType = typeInstantiation.getType(base.getType());
        newParams = createParams(base.getParams(), typeInstantiation);
        newMemberOf = memberOf;
        newInstantiations = base.getParameters().isEmpty() ? Collections.emptyMap() : new MapMaker().concurrencyLevel(1).weakValues().makeMap();
        newSignature = new Signature(this);
    }

    private static ImmutableList<FParameter> createParams(ImmutableList<FParameter> original, TypeInstantiation typeInstantiation) {
        ImmutableList.Builder<FParameter> res = ImmutableList.builder();
        for (FParameter p : original) {
            res.add(FParameter.create(p.getIdentifier(), typeInstantiation.getType(p.getType()), p.hasDefaultValue()));
        }
        return res.build();
    }

    public static ClassInstantiationFunction fromClassInstantiation(FInstantiatedClass _class, FFunction base) {
        return new ClassInstantiationFunction(base, _class);
    }


    @Override
    public FInstantiatedClass getMemberOf() {
        return newMemberOf;
    }

    @Override
    public Optional<FBlock> getBody() {
        return Optional.ofNullable(newBody);
    }

    @Override
    public ImmutableList<FParameter> getParams() {
        return newParams;
    }

    @Override
    public FType getType() {
        return newReturnType;
    }

    @Override
    public Signature getSignature() {
        return newSignature;
    }

    @Override
    public boolean isInstantiation() {
        return true;
    }

    @Override
    public FFunction getBaseR() {
        return proxy.getBaseR();
    }

    @Override
    public TypeInstantiation getTypeInstantiationToBase() {
        return proxy.getTypeInstantiationToBase().then(newMemberOf.getTypeInstantiation());
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) { //TODO this is a copy of baseFunctions impl, but we can't proxy it because we need to work on our own copy of the data
        TypeInstantiation intersected = typeInstantiation.intersect(getParametersList());
        if (intersected.isEmpty())
            return this;
        return newInstantiations.computeIfAbsent(intersected, i -> FInstantiatedFunction.fromFunctionInstantiation(this, intersected));
    }

    public boolean isBaked() {
        return baked;
    }

    @Override
    public void setBody(FBlock body) {
        newBody = body;
        baked = true;
    }

    public void setBaked() {
        baked = true;
    }
}

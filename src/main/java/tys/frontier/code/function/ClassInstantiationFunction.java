package tys.frontier.code.function;

import com.google.common.collect.MapMaker;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FInstantiatedClass;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;

public class ClassInstantiationFunction extends WithInstantiatedSignature {

    private FBlock newBody;
    private FInstantiatedClass newMemberOf;

    private Map<TypeInstantiation, FInstantiatedFunction> newInstantiations;

    private boolean baked = false;

    private ClassInstantiationFunction(FFunction base, FInstantiatedClass memberOf) {
        super(base, memberOf.getTypeInstantiation());
        assert base.getMemberOf() == memberOf.getProxy().getNamespace();
        assert (base instanceof FBaseFunction);
        assert getTypeInstantiation().intersect(base.getParameters().values()).isEmpty();
        newMemberOf = memberOf;
        newInstantiations = base.getParameters().isEmpty() ? Collections.emptyMap() : new MapMaker().concurrencyLevel(1).weakValues().makeMap();
    }

    public static ClassInstantiationFunction fromClassInstantiation(FInstantiatedClass _class, FFunction base) {
        return new ClassInstantiationFunction(base, _class);
    }


    @Override
    public DefaultNamespace getMemberOf() {
        return newMemberOf.getNamespace();
    }

    @Override
    public Optional<FBlock> getBody() {
        return Optional.ofNullable(newBody);
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) { //TODO this is a copy of baseFunctions impl, but we can't proxy it because we need to work on our own copy of the data
        TypeInstantiation intersected = typeInstantiation.intersect(getParameters().values());
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

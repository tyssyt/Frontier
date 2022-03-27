package tys.frontier.code.function;

import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FInstantiatedFunctionIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.passes.GenericBaking;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class FInstantiatedFunction extends WithInstantiatedSignature {

    private FBlock newBody;
    private FIdentifier newIdentifier;

    private boolean baked = false;

    private FInstantiatedFunction(FFunction base, TypeInstantiation typeInstantiation) {
        super(base, typeInstantiation);
        assert !(base instanceof FInstantiatedFunction);
        assert typeInstantiation.fits(base);
        newIdentifier = new FInstantiatedFunctionIdentifier(base.getIdentifier(), typeInstantiation);
    }

    static FInstantiatedFunction fromFunctionInstantiation(FFunction base, TypeInstantiation typeInstantiation) {
        return new FInstantiatedFunction(base, typeInstantiation);
    }

    @Override
    public DefaultNamespace getMemberOf() {
        return (DefaultNamespace) super.getMemberOf();
    }

    @Override
    public Optional<FBlock> getBody() {
        return Optional.ofNullable(newBody);
    }

    @Override
    public FIdentifier getIdentifier() {
        return newIdentifier;
    }

    @Override
    public Map<FIdentifier, FTypeVariable> getParameters() {
        return Collections.emptyMap();
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) {
        //TODO a more elegant (but maybe slightly slower) solution would be this.typeInstantiation.then(typeInstantiation).intersect(proxy.getParametersList())
        Map<FTypeVariable, FType> baseMap = new HashMap<>(proxy.getParameters().size());
        for (FTypeVariable var : proxy.getParameters().values())
            baseMap.put(var, typeInstantiation.getType(getTypeInstantiation().getType(var)));
        return proxy.getInstantiation(TypeInstantiation.create(baseMap));
    }

    public boolean isBaked() {
        return baked;
    }

    @Override
    public void setBody(FBlock body) {
        newBody = body;
        baked = true;
    }

    public void bake() {
        if (baked)
            return;
        GenericBaking.bake(this);
    }

    public void setBaked() {
        baked = true;
    }
}

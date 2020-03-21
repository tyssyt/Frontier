package tys.frontier.code.function;

import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FInstantiatedFunctionIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.passes.GenericBaking;

import java.util.*;

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
    public List<FTypeVariable> getParametersList() {
        return Collections.emptyList();
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) {
        /* TODO this optimization has to be removed, because when expression baking, we might have an empty type
             instantiation but still need to run all parameters through a getType to replace resolved variables
        if (typeInstantiation.isEmpty())
            return this;
        */
        //TODO a more elegant solution would be this.typeInstantiation.then(typeInstantiation).intersect(proxy.getParametersList())
        Map<FTypeVariable, FType> baseMap = new HashMap<>(proxy.getParametersList().size());
        for (FTypeVariable var : proxy.getParametersList()) {
            FType type = typeInstantiation.getType(getTypeInstantiation().getType(var));
            baseMap.put(proxy.getParameters().get(var.getIdentifier()), type);
        }
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

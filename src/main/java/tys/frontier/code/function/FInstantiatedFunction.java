package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FInstantiatedFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.passes.GenericBaking;

import java.util.*;

public class FInstantiatedFunction extends ForwardingFunction {

    private TypeInstantiation typeInstantiation;

    private FBlock newBody;
    private FFunctionIdentifier newIdentifier;
    private FType newReturnType;
    private ImmutableList<FParameter> newParams;

    private boolean baked = false;

    private FInstantiatedFunction(FFunction base, TypeInstantiation typeInstantiation) {
        super(base);
        assert !(base instanceof FInstantiatedFunction);
        assert typeInstantiation.fits(base);
        this.typeInstantiation = typeInstantiation;
        newIdentifier = new FInstantiatedFunctionIdentifier(base.getIdentifier(), typeInstantiation);
        newReturnType = typeInstantiation.getType(base.getType());
        newParams = createParams(base.getParams(), typeInstantiation);
    }

    private static ImmutableList<FParameter> createParams(ImmutableList<FParameter> original, TypeInstantiation typeInstantiation) {
        ImmutableList.Builder<FParameter> res = ImmutableList.builder();
        for (FParameter p : original) {
            res.add(FParameter.create(p.getIdentifier(), typeInstantiation.getType(p.getType()), p.hasDefaultValue()));
        }
        return res.build();
    }

    static FInstantiatedFunction fromFunctionInstantiation(FFunction base, TypeInstantiation typeInstantiation) {
        return new FInstantiatedFunction(base, typeInstantiation);
    }

    @Override
    public ImmutableList<FParameter> getParams() {
        return newParams;
    }

    @Override
    public Optional<FBlock> getBody() {
        return Optional.ofNullable(newBody);
    }

    @Override
    public FFunctionIdentifier getIdentifier() {
        return newIdentifier;
    }

    @Override
    public FType getType() {
        return newReturnType;
    }

    @Override
    public Map<FTypeIdentifier, FTypeVariable> getParameters() {
        return Collections.emptyMap();
    }

    @Override
    public List<FTypeVariable> getParametersList() {
        return Collections.emptyList();
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
        return proxy.getTypeInstantiationToBase().then(typeInstantiation);
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
            FType type = typeInstantiation.getType(this.typeInstantiation.getType(var));
            baseMap.put(proxy.getParameters().get(var.getIdentifier()), type);
        }
        return proxy.getInstantiation(TypeInstantiation.create(baseMap));
    }

    private boolean sane(TypeInstantiation other) {
        for (Map.Entry<FTypeVariable, FType> entry : typeInstantiation.getTypeMap().entrySet()) {
            FTypeVariable var = entry.getKey();
            if (other.contains(var) && other.getType(var) != entry.getValue())
                return false;
        }
        return true;
    }

    public TypeInstantiation getTypeInstantiation() {
        return typeInstantiation;
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

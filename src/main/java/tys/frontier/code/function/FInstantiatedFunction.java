package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FInstantiatedFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.statement.FBlock;
import tys.frontier.passes.GenericBaking;

import java.util.*;

public class FInstantiatedFunction extends ForwardingFunction {

    private TypeInstantiation typeInstantiation;

    private FBlock newBody;
    private FFunctionIdentifier newIdentifier;
    private FType newReturnType;
    private ImmutableList<FParameter> newParams;
    private Signature newSignature;
    private List<FFunctionCall> newCalledBy = new ArrayList<>();

    private boolean baked = false;

    private FInstantiatedFunction(FFunction base, TypeInstantiation typeInstantiation) {
        super(base);
        assert !(base instanceof ClassInstantiationFunction);
        assert typeInstantiation.fits(base);
        this.typeInstantiation = typeInstantiation;
        newIdentifier = new FInstantiatedFunctionIdentifier(base.getIdentifier(), typeInstantiation);
        newReturnType = typeInstantiation.getType(base.getType());
        newParams = createParams(base.getParams(), typeInstantiation);
        newSignature = new Signature(this);
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
    public List<FFunctionCall> getCalledBy() {
        return newCalledBy;
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
    public Signature getSignature() {
        return newSignature;
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
        if (typeInstantiation.isEmpty())
            return this;
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
    public boolean addCall(FFunctionCall call) {
        proxy.addCall(call);
        return newCalledBy.add(call);
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
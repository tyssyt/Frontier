package tys.frontier.code.type;

import com.google.common.collect.BiMap;
import com.google.common.collect.ListMultimap;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;

import java.util.List;
import java.util.Map;

public abstract class FForwardingClass implements FClass {

    protected FClass proxy;

    public FForwardingClass(FClass proxy) {
        this.proxy = proxy;
    }

    public FClass getProxy() {
        return proxy;
    }

    @Override
    public void setParameters(List<FTypeVariable> parameters, List<Variance> parameterVariance) {
        proxy.setParameters(parameters, parameterVariance);
    }

    @Override
    public long concreteness() {
        return proxy.concreteness();
    }

    @Override
    public FIdentifier getIdentifier() {
        return proxy.getIdentifier();
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return proxy.getVisibility();
    }

    @Override
    public FVisibilityModifier getConstructorVisibility() {
        return proxy.getConstructorVisibility();
    }

    @Override
    public void setConstructorVisibility(FVisibilityModifier constructorVisibility) {
        proxy.setConstructorVisibility(constructorVisibility);
    }

    @Override
    public BiMap<FIdentifier, FField> getInstanceFields() {
        return proxy.getInstanceFields();
    }

    @Override
    public BiMap<FIdentifier, FField> getStaticFields() {
        return proxy.getStaticFields();
    }

    @Override
    public ListMultimap<FIdentifier, Signature> getFunctions(boolean lhsSignatures) {
        return proxy.getFunctions(lhsSignatures);
    }

    @Override
    public List<? extends FType> getParametersList() {
        return proxy.getParametersList();
    }

    @Override
    public Variance getParameterVariance(FTypeVariable parameter) {
        return proxy.getParameterVariance(parameter);
    }

    @Override
    public Variance getParameterVariance(int i) {
        return proxy.getParameterVariance(i);
    }

    @Override
    public FClass getInstantiation(List<FType> types) throws WrongNumberOfTypeArguments {
        return proxy.getInstantiation(types);
    }

    @Override
    public Map<FType, FField> getDirectDelegates() {
        return proxy.getDirectDelegates();
    }

    @Override
    public FIdentifier getFreshLambdaName() {
        return proxy.getFreshLambdaName();
    }
}

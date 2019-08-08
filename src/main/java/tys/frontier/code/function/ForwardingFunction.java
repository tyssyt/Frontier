package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public abstract class ForwardingFunction implements FFunction {

    protected FFunction proxy;

    protected ForwardingFunction(FFunction proxy) {
        this.proxy = proxy;
    }

    public FFunction getProxy() {
        return proxy;
    }

    @Override
    public boolean isInstance() {
        return proxy.isInstance();
    }

    @Override
    public FType getMemberOf() {
        return proxy.getMemberOf();
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return proxy.getVisibility();
    }

    @Override
    public boolean isNative() {
        return proxy.isNative();
    }

    @Override
    public ImmutableList<FParameter> getParams() {
        return proxy.getParams();
    }

    @Override
    public Optional<FBlock> getBody() {
        return proxy.getBody();
    }

    @Override
    public void setBody(FBlock body) {
        proxy.setBody(body);
    }

    @Override
    public FFunctionIdentifier getIdentifier() {
        return proxy.getIdentifier();
    }

    @Override
    public FType getType() {
        return proxy.getType();
    }

    @Override
    public MemberType getMemberType() {
        return proxy.getMemberType();
    }

    @Override
    public boolean isConstructor() {
        return proxy.isConstructor();
    }

    @Override
    public boolean isPredefined() {
        return proxy.isPredefined();
    }

    @Override
    public Signature getSignature() {
        return proxy.getSignature();
    }

    @Override
    public boolean isMain() {
        return proxy.isMain();
    }

    @Override
    public FLocalVariable getFreshVariable(FType type) {
        return proxy.getFreshVariable(type);
    }

    @Override
    public Map<FTypeIdentifier, FTypeVariable> getParameters() {
        return proxy.getParameters();
    }

    @Override
    public List<FTypeVariable> getParametersList() {
        return proxy.getParametersList();
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) {
        return proxy.getInstantiation(typeInstantiation);
    }

    @Override
    public String toString() {
        return tS();
    }
}

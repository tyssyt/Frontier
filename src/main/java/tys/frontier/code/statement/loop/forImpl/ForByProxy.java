package tys.frontier.code.statement.loop.forImpl;

import tys.frontier.code.function.FFunction;
import tys.frontier.code.type.FType;

public class ForByProxy implements ForImpl {

    private FFunction getProxy;

    public ForByProxy(FFunction getProxy) {
        this.getProxy = getProxy;
    }

    public FFunction getGetProxy() {
        return getProxy;
    }

    public ForImpl getProxyImpl() {
        return getProxy.getType().getForImpl();
    }

    @Override
    public FType getElementType() {
        return getProxyImpl().getElementType();
    }
}

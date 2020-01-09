package tys.frontier.code.statement.loop.forImpl;

import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.util.Utils;

public class FTypeVariableForImpl implements ForImpl {

    private FTypeVariable typeVariable;

    public FTypeVariableForImpl(FTypeVariable typeVariable) {
        this.typeVariable = typeVariable;
    }

    public FTypeVariable getTypeVariable() {
        return typeVariable;
    }

    @Override
    public FType getElementType() {
        return Utils.NYI("FTypeVariableForImpl.getElementType()");
    }
}

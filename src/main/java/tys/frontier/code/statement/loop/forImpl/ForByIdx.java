package tys.frontier.code.statement.loop.forImpl;

import tys.frontier.code.function.FFunction;
import tys.frontier.code.type.FType;

public class ForByIdx implements ForImpl {

    private FFunction getElement;
    private FFunction getSize;

    public ForByIdx(FFunction getElement, FFunction getSize) {
        this.getElement = getElement;
        this.getSize = getSize;
    }

    public FFunction getGetElement() {
        return getElement;
    }

    public FFunction getGetSize() {
        return getSize;
    }

    @Override
    public FType getElementType() {
        return getElement.getType();
    }
}

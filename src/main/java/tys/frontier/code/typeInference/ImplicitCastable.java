package tys.frontier.code.typeInference;

import tys.frontier.code.FType;

public class ImplicitCastable implements TypeConstraint {

    private FType to;

    public ImplicitCastable(FType to) {
        this.to = to;
    }

    public FType getTo() {
        return to;
    }
}
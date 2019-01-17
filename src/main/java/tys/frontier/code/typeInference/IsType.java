package tys.frontier.code.typeInference;

import tys.frontier.code.FType;

public class IsType implements TypeConstraint {

    private FType target;

    public IsType(FType target) {
        this.target = target;
    }

    public FType getTarget() {
        return target;
    }
}

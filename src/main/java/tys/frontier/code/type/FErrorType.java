package tys.frontier.code.type;

import tys.frontier.code.identifier.FIdentifier;

public class FErrorType implements FType {

    public final FIdentifier identifier;

    public FErrorType(FIdentifier identifier) {
        this.identifier = identifier;
    }

    @Override
    public String toString() {
        return "ERROR(" + identifier +')';
    }
}

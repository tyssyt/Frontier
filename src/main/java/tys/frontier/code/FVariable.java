package tys.frontier.code;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;

public abstract class FVariable implements IdentifierNameable, Typed {

    protected FIdentifier identifier;
    private FType type;

    public FVariable(FIdentifier identifier, FType type) {
        this.identifier = identifier;
        this.type = type;
    }

    @Override
    public FIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FType getType() {
        return type;
    }

    public void setType(FType type) {
        this.type = type;
    }

    @Override
    public String toString() {
        return type.getIdentifier() + " " + identifier;
    }
}

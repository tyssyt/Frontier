package tys.frontier.code;

import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;

public abstract class FVariable implements IdentifierNameable, Typed {

    private FVariableIdentifier identifier;
    private FType type;

    public FVariable(FVariableIdentifier identifier, FType type) {
        this.identifier = identifier;
        this.type = type;
    }

    @Override
    public FVariableIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FType getType() {
        return type;
    }

    @Override
    public String toString() {
        return type.getIdentifier() + " " + identifier;
    }
}

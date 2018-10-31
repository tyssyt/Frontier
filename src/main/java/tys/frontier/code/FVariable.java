package tys.frontier.code;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.predefinedClasses.FTypeType;

public abstract class FVariable implements IdentifierNameable, Typed {

    private FIdentifier identifier;
    private FType type;

    public FVariable(FIdentifier identifier, FType type) {
        assert identifier instanceof FVariableIdentifier || (type == FTypeType.INSTANCE && identifier instanceof FTypeIdentifier); //TODO I thinks this should be a syntax error instead of an assertion
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

    @Override
    public String toString() {
        return type.getIdentifier() + " " + identifier;
    }
}

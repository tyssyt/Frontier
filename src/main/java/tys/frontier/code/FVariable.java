package tys.frontier.code;

import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;

public class FVariable implements IdentifierNameable, Typed {

    private FVariableIdentifier identifier;
    private FClass type;

    public FVariable(FVariableIdentifier identifier, FClass type) {
        this.identifier = identifier;
        this.type = type;
    }

    @Override
    public FVariableIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FClass getType() {
        return type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FVariable fVariable = (FVariable) o;

        if (!identifier.equals(fVariable.identifier)) return false;
        return type.equals(fVariable.type);
    }

    @Override
    public int hashCode() {
        return 31 * identifier.hashCode() + type.hashCode();
    }

    @Override
    public String toString() {
        return type + " " + identifier;
    }
}

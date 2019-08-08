package tys.frontier.code;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;

public class FLocalVariable extends FVariable {

    public FLocalVariable(FIdentifier identifier, FType type) {
        super(identifier, type);
    }
}

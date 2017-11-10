package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FVariableIdentifier;

public class FieldNotFound extends SyntaxError {

    public final FVariableIdentifier identifier;

    public FieldNotFound(FVariableIdentifier identifier) {
        super(identifier.toString());
        this.identifier = identifier;
    }
}

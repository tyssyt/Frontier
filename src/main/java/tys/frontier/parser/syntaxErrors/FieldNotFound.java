package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;

public class FieldNotFound extends SyntaxError {

    public final FIdentifier identifier;

    public FieldNotFound(FIdentifier identifier) {
        super(identifier.toString());
        this.identifier = identifier;
    }
}

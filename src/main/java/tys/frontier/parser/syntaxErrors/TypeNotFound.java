package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;

public class TypeNotFound extends SyntaxError {

    public final FIdentifier identifier;

    public TypeNotFound(FIdentifier identifier) {
        super(identifier.toString());
        this.identifier = identifier;
    }
}

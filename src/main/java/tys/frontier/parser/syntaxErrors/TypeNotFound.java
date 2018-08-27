package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FTypeIdentifier;

public class TypeNotFound extends SyntaxError {

    public final FTypeIdentifier identifier;

    public TypeNotFound(FTypeIdentifier identifier) {
        super(identifier.toString());
        this.identifier = identifier;
    }
}

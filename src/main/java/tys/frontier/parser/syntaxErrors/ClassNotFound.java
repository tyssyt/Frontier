package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;

public class ClassNotFound extends SyntaxError {

    public final FIdentifier identifier;

    public ClassNotFound(FIdentifier identifier) {
        super(identifier.toString());
        this.identifier = identifier;
    }
}

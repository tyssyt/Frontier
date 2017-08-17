package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;

public class ClassNotFound extends SyntaxError {

    private FIdentifier identifier;

    public ClassNotFound(FIdentifier identifier) {
        super(identifier.toString());
        this.identifier = identifier;
    }
}

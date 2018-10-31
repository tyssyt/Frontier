package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;

public class UndeclaredVariable extends SyntaxError {

    public final FIdentifier identifier;

    public UndeclaredVariable(FIdentifier identifier) {
        super(identifier.toString());
        this.identifier = identifier;
    }
}

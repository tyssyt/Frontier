package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;

public class UntypedVariable extends SyntaxError {

    public final FIdentifier identifier;

    public UntypedVariable(FIdentifier identifier) {
        super("untyped Variable: " + identifier);
        this.identifier = identifier;
    }
}

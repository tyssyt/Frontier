package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FVariableIdentifier;

public class UntypedVariable extends SyntaxError {

    public final FVariableIdentifier identifier;

    public UntypedVariable(FVariableIdentifier identifier) {
        super("untyped Variable: " + identifier);
        this.identifier = identifier;
    }
}

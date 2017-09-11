package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.identifier.FVariableIdentifier;

public class UndeclaredVariable extends SyntaxError {

    public final FVariableIdentifier identifier;

    public UndeclaredVariable(FVariableIdentifier identifier) {
        super(identifier.toString());
        this.identifier = identifier;
    }
}

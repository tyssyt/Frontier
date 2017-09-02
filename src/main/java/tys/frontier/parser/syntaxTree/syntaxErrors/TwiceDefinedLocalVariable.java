package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.identifier.FVariableIdentifier;

public class TwiceDefinedLocalVariable extends SyntaxError {

    public final FVariableIdentifier identifier;

    public TwiceDefinedLocalVariable(FVariableIdentifier identifier) {
        super(identifier.toString());
        this.identifier = identifier;
    }
}

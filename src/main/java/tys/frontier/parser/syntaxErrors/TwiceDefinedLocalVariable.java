package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;

public class TwiceDefinedLocalVariable extends SyntaxError {

    public final FIdentifier identifier;

    public TwiceDefinedLocalVariable(FIdentifier identifier) {
        super(identifier.toString());
        this.identifier = identifier;
    }
}

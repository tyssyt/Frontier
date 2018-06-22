package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FFunction;

public class NoOverride extends SyntaxError {

    public final FFunction fFunction;

    public NoOverride(FFunction fFunction) {
        super(fFunction.getIdentifier() + " does not override any function");
        this.fFunction = fFunction;
    }
}

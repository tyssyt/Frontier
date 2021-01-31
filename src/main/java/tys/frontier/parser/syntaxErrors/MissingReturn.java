package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.function.FFunction;

public class MissingReturn extends SyntaxError {

    public final FFunction function;

    public MissingReturn(FFunction function) {
        super(function.getLocation().getPoint(), function.headerToString() + " does not always return a Value");
        this.function = function;
    }
}

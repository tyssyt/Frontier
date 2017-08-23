package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.FFunction;

public class StaticAccessToInstanceFunction extends SyntaxError {

    public final FFunction function;

    public StaticAccessToInstanceFunction(FFunction function) {
        super(function.toString());
        this.function = function;
    }
}

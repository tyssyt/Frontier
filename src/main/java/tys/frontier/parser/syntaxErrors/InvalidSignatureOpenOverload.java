package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.function.FFunction;

public class InvalidSignatureOpenOverload extends SyntaxError {

    public final FFunction _function;
    public final FFunction openRemote;

    public InvalidSignatureOpenOverload(FFunction _function, FFunction openRemote) {
        super("Invalid remote function declaration: " + _function + ", does not match remote function: " + openRemote);
        this._function = _function;
        this.openRemote = openRemote;
    }
}

package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.function.FFunction;

public class NativeWithBody extends SyntaxError {

    public final FFunction _function;

    public NativeWithBody(FFunction _function) {
        super(_function.getLocation().getPoint(), "native funcion has body: " + _function);
        this._function = _function;
    }
}

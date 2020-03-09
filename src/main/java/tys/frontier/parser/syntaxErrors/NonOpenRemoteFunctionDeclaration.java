package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.function.FFunction;

public class NonOpenRemoteFunctionDeclaration extends SyntaxError {

    public final FFunction _function;

    public NonOpenRemoteFunctionDeclaration(FFunction _function, String reason) {
        super("Invalid remote funcion declaration, remote does not declare signature as open: " + _function);
        this._function = _function;
    }
}

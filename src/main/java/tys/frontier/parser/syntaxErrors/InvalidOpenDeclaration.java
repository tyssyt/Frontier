package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.function.FFunction;

public class InvalidOpenDeclaration extends SyntaxError {

    public final FFunction _function;

    public InvalidOpenDeclaration(FFunction _function, String reason) {
        super("invalid open declaration, " + reason + ": " + _function);
        this._function = _function;
    }
}

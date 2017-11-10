package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FFunction;

public class FunctionNotFound extends SyntaxError {

    public final FFunction.Signature signature;

    public FunctionNotFound(FFunction.Signature signature) {
        super(signature.toString());
        this.signature = signature;
    }
}

package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.expression.FFunctionCall;

public class AbstractClassConstructorCall extends SyntaxError {

    public final FFunctionCall constructorCall;

    public AbstractClassConstructorCall(FFunctionCall constructorCall) {
        super("constructor of " + constructorCall.getFunction().getClazz().getIdentifier() + " is called, but the class is abstract");
        this.constructorCall = constructorCall;
    }
}

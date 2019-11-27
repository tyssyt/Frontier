package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.type.FType;

public class NotEnoughArguments extends SyntaxError {

    public final FType missing;

    public NotEnoughArguments(String message, FType missing) {
        super(message + " Missing at: " + missing);
        this.missing = missing;
    }
}

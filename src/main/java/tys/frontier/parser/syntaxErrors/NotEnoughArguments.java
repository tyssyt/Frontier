package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.type.FType;
import tys.frontier.parser.location.Position;

public class NotEnoughArguments extends SyntaxError {

    public final FType missing;

    public NotEnoughArguments(Position position, String message, FType missing) {
        super(position, message + " Missing at: " + missing);
        this.missing = missing;
    }

    public NotEnoughArguments(String message, FType missing) {
        super(message + " Missing at: " + missing);
        this.missing = missing;
    }
}

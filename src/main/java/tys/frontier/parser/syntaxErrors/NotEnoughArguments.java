package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FParameter;
import tys.frontier.parser.location.Position;

public class NotEnoughArguments extends SyntaxError {

    public final FParameter missing;

    public NotEnoughArguments(Position position, String message, FParameter missing) {
        super(position, message + " Missing at: " + missing);
        this.missing = missing;
    }

    public NotEnoughArguments(String message, FParameter missing) {
        super(message + " Missing at: " + missing);
        this.missing = missing;
    }
}

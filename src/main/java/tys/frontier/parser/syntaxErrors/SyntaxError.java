package tys.frontier.parser.syntaxErrors;

import tys.frontier.parser.location.Position;

public class SyntaxError extends Exception {

    //TODO needs to be location instead of just Position
    public final Position position;
    public final Position position2;

    //TODO this needs to be removed with all calls getting a Position
    public SyntaxError(String message) {
        super(message);
        this.position = null;
        this.position2 = null;
    }

    public SyntaxError(Position position, String message) {
        super(message + (position != null ? " at "  +  position : ""));
        this.position = position;
        this.position2 = null;
    }

    public SyntaxError(Position position, Position position2, String message) {
        super(message + " at " + position + " and " + position2);
        this.position = position;
        this.position2 = position2;
    }

    public SyntaxError(Position position, String message, Throwable cause) {
        super(message + " at " + position, cause);
        this.position = position;
        this.position2 = null;
    }

    public SyntaxError(Throwable cause) {
        super(cause);
        position = null;
        position2 = null;
    }
}

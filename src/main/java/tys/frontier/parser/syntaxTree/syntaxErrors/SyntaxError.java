package tys.frontier.parser.syntaxTree.syntaxErrors;

public class SyntaxError extends Exception {
    public SyntaxError() {
    }

    public SyntaxError(String message) {
        super(message);
    }

    public SyntaxError(String message, Throwable cause) {
        super(message, cause);
    }

    public SyntaxError(Throwable cause) {
        super(cause);
    }
}

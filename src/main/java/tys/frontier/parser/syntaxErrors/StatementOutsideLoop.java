package tys.frontier.parser.syntaxErrors;

import tys.frontier.parser.location.Position;

public class StatementOutsideLoop extends SyntaxError {
    public StatementOutsideLoop(Position position) {
        super(position, "Loop Control Statement outside of Loop");
    }
}

package tys.frontier.parser.syntaxErrors;

import tys.frontier.parser.location.Position;

public class InvalidPath extends SyntaxError {

    public final String path;

    public InvalidPath(Position position, String path) {
        super(position, "Invalid path specification. if the path contains a \"*\" or \"**\", the characters afterwards specifiy a filter on file extension and must start with a \".\"");
        this.path = path;
    }
}

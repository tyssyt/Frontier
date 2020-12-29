package tys.frontier.parser.syntaxErrors;

public class InvalidPath extends SyntaxError {

    public final String path;

    public InvalidPath(String path) {
        super("Invalid path specification. if the path contains a \"*\" or \"**\", the characters afterwards specifiy a filter on file extension and must start with a \".\"");
        this.path = path;
    }
}

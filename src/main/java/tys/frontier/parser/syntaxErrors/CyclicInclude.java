package tys.frontier.parser.syntaxErrors;

import java.nio.file.Path;

public class CyclicInclude extends SyntaxError {

    public final Path path;

    public CyclicInclude(Path path) {
        super("Cyclic Include starting at: " + path);
        this.path = path;
    }
}

package tys.frontier;

import tys.frontier.parser.Parser;
import tys.frontier.parser.dependencies.ImportResolver;

public class State {

    private static State instance = new State();

    public static State get() {
        return instance;
    }

    private Parser currentParser = null;
    private ImportResolver importResolver;

    public Parser getCurrentParser() {
        return currentParser;
    }

    public Parser setCurrentParser(Parser currentParser) {
        Parser old = this.currentParser;
        this.currentParser = currentParser;
        return old;
    }

    public ImportResolver getImportResolver() {
        return importResolver;
    }

    public void setImportResolver(ImportResolver importResolver) {
        this.importResolver = importResolver;
    }
}

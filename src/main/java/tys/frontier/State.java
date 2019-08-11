package tys.frontier;

import tys.frontier.parser.Parser;

public class State {

    private static State instance = new State();

    public static State get() {
        return instance;
    }

    private Parser currentParser;
    private Parser entryPointParser;

    public Parser getCurrentParser() {
        return currentParser;
    }

    public void setCurrentParser(Parser currentParser) {
        assert this.currentParser == null || currentParser == null;
        this.currentParser = currentParser;
    }

    public Parser getEntryPointParser() {
        return entryPointParser;
    }

    public void setEntryPointParser(Parser entryPointParser) {
        assert this.entryPointParser == null || entryPointParser == null;
        this.entryPointParser = entryPointParser;
    }
}

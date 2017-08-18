package tys.frontier.code.statement.loop;

import tys.frontier.code.statement.FStatement;

public class FContinue implements FStatement {

    private FLoopIdentifier loop;

    public FContinue(FLoopIdentifier loop) {
        this.loop = loop;
    }

    public FLoopIdentifier getLoop() {
        return loop;
    }
}

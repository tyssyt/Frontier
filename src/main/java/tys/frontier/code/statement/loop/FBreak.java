package tys.frontier.code.statement.loop;

import tys.frontier.code.statement.FStatement;

public class FBreak implements FStatement {

    private FLoopIdentifier loop;

    public FBreak(FLoopIdentifier loop) {
        this.loop = loop;
    }

    public FLoopIdentifier getLoop() {
        return loop;
    }
}

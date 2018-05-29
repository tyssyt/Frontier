package tys.frontier.code.statement.loop;

import tys.frontier.code.statement.ControlFlowIDontKnow;
import tys.frontier.code.statement.FStatement;

import java.util.Optional;

public abstract class FLoop implements FStatement, ControlFlowIDontKnow {

    private int nestedDepth;
    private FLoopIdentifier identifier;
    private FStatement body;

    public FLoop(int nestedDepth, FLoopIdentifier identifier, FStatement body) {
        this.nestedDepth = nestedDepth;
        this.identifier = identifier;
        this.body = body;
    }

    public int getNestedDepth() {
        return nestedDepth;
    }

    public FLoopIdentifier getIdentifier() {
        return identifier;
    }

    public FStatement getBody() {
        return body;
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return body.redirectsControlFlow().filter(x -> !x.equals(this));
    }

}

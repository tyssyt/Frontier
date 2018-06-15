package tys.frontier.code.statement.loop;

import tys.frontier.code.statement.ControlFlowIDontKnow;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;

import java.util.Optional;

public abstract class FLoop implements FStatement, ControlFlowIDontKnow {

    private int nestedDepth;
    private FLoopIdentifier identifier;
    private FBlock body;

    public FLoop(int nestedDepth, FLoopIdentifier identifier, FBlock body) {
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

    public FBlock getBody() {
        return body;
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return body.redirectsControlFlow().filter(x -> !x.equals(this));
    }

}

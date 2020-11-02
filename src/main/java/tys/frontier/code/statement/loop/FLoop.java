package tys.frontier.code.statement.loop;

import tys.frontier.code.statement.ControlFlowIDontKnow;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.parser.location.Position;

import java.util.Optional;

public abstract class FLoop extends FStatement implements ControlFlowIDontKnow {

    private int nestedDepth;
    private FLoopIdentifier identifier;
    private FBlock body;

    public FLoop(Position position, int nestedDepth, FLoopIdentifier identifier, FBlock body) {
        super(position);
        this.nestedDepth = nestedDepth;
        this.identifier = identifier;
        this.body = body;
        identifier.setLoop(this);
    }

    public int getNestedDepth() {
        return nestedDepth;
    }

    public FLoopIdentifier getIdentifier() {
        return identifier;
    }

    public void setBody(FBlock body) {
        assert this.body == null;
        this.body = body;
    }

    public FBlock getBody() {
        return body;
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return body.redirectsControlFlow().filter(x -> !x.equals(this));
    }

}

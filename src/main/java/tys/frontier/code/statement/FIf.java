package tys.frontier.code.statement;

import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.statement.loop.FLoop;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.Optional;

public class FIf implements FStatement {

    private FExpression condition;
    private FBlock then;
    private FBlock elze; //Optional

    private FIf(FExpression condition, FBlock then, FBlock elze) throws IncompatibleTypes {
        this.condition = condition;
        this.then = then;
        this.elze = elze;
        checkTypes();
    }

    public static FIf create(FExpression condition, FBlock then, FBlock elze) throws IncompatibleTypes {
        return new FIf(condition, then, elze);
    }
    public static FIf createTrusted(FExpression condition, FBlock then, FBlock elze) {
        try {
            return create(condition, then, elze);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public FExpression getCondition() {
        return condition;
    }

    public void setThen(FBlock then) {
        assert this.then == null;
        this.then = then;
    }

    public FBlock getThen() {
        return then;
    }

    public void setElse(FBlock _else) {
        assert this.elze == null;
        this.elze = _else;
    }

    public Optional<FBlock> getElse() {
        return Optional.ofNullable(elze);
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        if (elze != null) {
            return then.redirectsControlFlow().flatMap(
                    t -> elze.redirectsControlFlow().map(
                    e -> {
                        if (t instanceof FFunction)
                            return e;
                        if (e instanceof FFunction)
                            return t;
                        FLoop tL = ((FLoop) t);
                        FLoop eL = ((FLoop) e);
                        if (tL.getNestedDepth() < eL.getNestedDepth())
                            return eL;
                        else
                            return tL;
                    }
                    )); //this is obviously by far the most readable way to write this ;)
        }
        return Optional.empty(); //the obvious way to make this more readable is to reduce it to a one liner with getElse and more Optionals :)
    }

    private void checkTypes() throws IncompatibleTypes {
        condition = condition.typeCheck(FBool.INSTANCE);
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterIf(this);
        return visitor.exitIf(this, condition.accept(visitor), then.accept(visitor), getElse().map(statement -> statement.accept(visitor)));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitIf(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("if (");
        condition.toString(sb).append(") then ");
        then.toString(sb);
        getElse().ifPresent(elze -> elze.toString(sb.append(" else ")));
        return sb;
    }
    @Override
    public String toString() {
        return tS();
    }
}

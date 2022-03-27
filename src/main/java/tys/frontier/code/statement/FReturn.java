package tys.frontier.code.statement;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.Optional;

public class FReturn extends FStatement {

    private FExpression expression;
    private FFunction function;

    private FReturn(Position position, FExpression expression, FFunction function) {
        super(position);
        this.function = function;
        this.expression = expression;
    }

    public static FReturn create(Position position, FExpression expression, FFunction function) throws IncompatibleTypes {
        return new FReturn(position, expression, function).checkTypes();
    }

    public static FReturn createTrusted(Position position, FExpression expression, FFunction function) {
        try {
            return create(position, expression, function);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public static FReturn createWithDelayedCheck(Position position, FExpression expression, FFunction function) {
        return new FReturn(position, expression, function);
    }

    public FReturn checkTypes() throws IncompatibleTypes {
        if (expression != null)
            this.expression = expression.typeCheck(function.getType());
        else if (function.getType() != FTuple.VOID)
            throw new IncompatibleTypes(position, function.getType(), FTuple.VOID);
        return this;
    }

    public Optional<FExpression> getExpression() {
        return Optional.ofNullable(expression);
    }

    public FFunction getFunction() {
        return function;
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return Optional.of(function);
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterReturn(this);
        return visitor.exitReturn(this, Optional.ofNullable(expression).map(e -> e.accept(visitor)));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitReturn(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        if (expression == null)
            return sb.append("return;");
        else
            return sb.append("return ").append(expression).append(';');
    }
}
package tys.frontier.code.statement;

import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.Optional;

public class FReturn  implements FStatement {

    private FExpression expression; //optional, null if function is void
    private FFunction function;

    private FReturn(FExpression expression, FFunction function) throws IncompatibleTypes {
        this.expression = expression;
        this.function = function;
        checkTypes();
    }

    public static FReturn create(FExpression expression, FFunction function) throws IncompatibleTypes {
        return new FReturn(expression, function);
    }
    public static FReturn createTrusted(FExpression expression, FFunction function) {
        try {
            return create(expression, function);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
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

    private void checkTypes() throws IncompatibleTypes {
        if (expression == null) {
            if (function.getType() != FVoid.INSTANCE)
                throw new IncompatibleTypes(function.getType(), FVoid.INSTANCE);
        } else {
            expression = expression.typeCheck(function.getType());
        }
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterReturn(this);
        return visitor.exitReturn(this, getExpression().map(expression -> expression.accept(visitor)));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitReturn(this);
    }

    @Override
    @SuppressWarnings("ResultOfMethodCallIgnored")
    public StringBuilder toString(StringBuilder sb) {
        sb.append("return ");
        getExpression().ifPresent(e -> e.toString(sb));
        return sb.append(';');
    }
}
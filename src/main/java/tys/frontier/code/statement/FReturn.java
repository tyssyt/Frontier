package tys.frontier.code.statement;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FImplicitCast;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

import java.util.Optional;

public class FReturn  implements FStatement, NeedsTypeCheck {

    private FExpression expression; //optional, null if function is void
    private FFunction function;

    public FReturn(FExpression expression, FFunction function) {
        this.expression = expression;
        this.function = function;
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
    public void checkTypes() throws IncompatibleTypes {
        FClass expressionType = getExpression().map(FExpression::getType).orElse(FVoid.INSTANCE);
        if (function.getType() != expressionType)
            if (expression == null)
                throw new IncompatibleTypes(function.getType(), FVoid.INSTANCE);
            else
                expression = new FImplicitCast(function.getType(), expression);
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

package tys.frontier.code.statement;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

import java.util.Optional;

public class FReturn  implements FStatement, NeedsTypeCheck {

    private FExpression expression;
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
    public void checkTypes() throws IncompatibleTypes {
        FClass expressionType = getExpression().map(FExpression::getType).orElse(FVoid.INSTANCE);
        if (function.getType() != expressionType)
            throw new IncompatibleTypes(function.getType(), expressionType);
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.enterReturn(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("return ");
        getExpression().ifPresent(e -> e.toString(sb));
        return sb.append(';');
    }
}

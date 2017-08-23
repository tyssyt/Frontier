package tys.frontier.code.statement;

import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

public class FReturn extends FExpressionStatement implements NeedsTypeCheck {

    //TODO in a return the expression is optional, sooo

    private FFunction function;

    public FReturn(FExpression expression, FFunction function) {
        super(expression);
        this.function = function;
    }

    public FFunction getFunction() {
        return function;
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (getExpression().getType() != function.getType())
            throw new IncompatibleTypes(function.getType(), getExpression().getType());
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.enterReturn(this);
    }
}

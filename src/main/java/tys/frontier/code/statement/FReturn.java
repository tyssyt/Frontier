package tys.frontier.code.statement;

import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FExpression;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

public class FReturn extends FExpressionStatement implements NeedsTypeCheck {

    private FFunction function;

    public FReturn(FExpression expression, FFunction function) {
        super(expression);
        this.function = function;
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (getExpression().getType() != function.getType())
            throw new IncompatibleTypes(function.getType(), getExpression().getType());
    }
}

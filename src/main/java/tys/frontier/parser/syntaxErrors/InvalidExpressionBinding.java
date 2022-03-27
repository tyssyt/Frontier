package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.expression.UnboundExpression;
import tys.frontier.code.type.FType;

public class InvalidExpressionBinding extends IncompatibleTypes {

    public final UnboundExpression expression;
    public final FType binding;

    public InvalidExpressionBinding(UnboundExpression expression, FType binding) {
        super(expression.getPosition(), "Expression " + expression + " could not be bound. Type given was: " + binding, expression.getType(), binding);
        this.expression = expression;
        this.binding = binding;
    }
}

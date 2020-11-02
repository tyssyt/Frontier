package tys.frontier.code.statement;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.parser.location.Position;

public class FVarDeclaration extends FVariableExpression {
    public FVarDeclaration(Position position, FLocalVariable variable) {
        super(position, variable);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return super.toString(sb).append(':').append(getType().getIdentifier().name);
    }
}

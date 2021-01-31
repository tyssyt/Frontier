package tys.frontier.code.statement;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FVariableExpression;

public class FVarDeclaration extends FVariableExpression {
    public FVarDeclaration(FLocalVariable variable) {
        super(variable.getPosition(), variable);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return super.toString(sb).append(':').append(getType().getIdentifier().name);
    }
}

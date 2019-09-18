package tys.frontier.code.statement;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FLocalVariableExpression;

public class FVarDeclaration extends FLocalVariableExpression {
    public FVarDeclaration(FLocalVariable variable) {
        super(variable);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return super.toString(sb).append(':').append(getType().getIdentifier().name);
    }
}

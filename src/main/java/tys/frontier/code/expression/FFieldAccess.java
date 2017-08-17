package tys.frontier.code.expression;

import tys.frontier.code.FField;
import tys.frontier.code.type.FType;

public class FFieldAccess implements FExpression {

    private FField field;

    @Override
    public FType getType() {
        return field.getType();
    }
}

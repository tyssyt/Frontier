package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;

public class FFieldAccess implements FExpression {

    private FField field;

    @Override
    public FClass getType() {
        return field.getType();
    }
}

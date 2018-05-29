package tys.frontier.code.expression;

import tys.frontier.code.FVariable;

public interface FVariableExpression extends FExpression {

    enum AccessType {
        LOAD,
        STORE,
        LOAD_AND_STORE
    }

    FVariable getVariable();
    AccessType getAccessType();
    void setAccessType(AccessType accessType);

}

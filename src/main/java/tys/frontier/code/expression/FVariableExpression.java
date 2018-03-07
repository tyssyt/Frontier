package tys.frontier.code.expression;

import tys.frontier.code.FVariable;

public interface FVariableExpression extends FExpression {

    enum AccessType {
        LOAD,
        STORE
    }

    FVariable getVariable();
    AccessType getAccessType();
    void setStore();

}

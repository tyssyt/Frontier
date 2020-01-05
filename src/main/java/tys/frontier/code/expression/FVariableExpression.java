package tys.frontier.code.expression;

import tys.frontier.code.FVariable;

public interface FVariableExpression extends FExpression {

    enum AccessType {
        LOAD,
        STORE
    }

    FVariable getVariable();
    AccessType getAccessType();
    void setAccessType(AccessType accessType);

    /**
     * Does not copy the access type
     * @return a fresh (not deep) copy of this
     */
    FVariableExpression copy();

}

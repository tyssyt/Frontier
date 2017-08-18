package tys.frontier.code.statement;

import tys.frontier.code.FVariable;

import java.util.Optional;

public class FVarDeclaration implements FStatement {

    private FVariable var;
    private FVarAssignment assignment; //optional

    public FVarDeclaration(FVariable var, FVarAssignment assignment) {
        this.var = var;
        this.assignment = assignment;
        assert assignment == null || var == assignment.getVariable();
    }

    public FVariable getVar() {
        return var;
    }

    public Optional<FVarAssignment> getAssignment() {
        return assignment == null ? Optional.empty() : Optional.of(assignment);
    }
}

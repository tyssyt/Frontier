package tys.frontier.code.statement;

import tys.frontier.code.FVariable;
import tys.frontier.code.visitor.StatementVisitor;

import java.util.Optional;

public class FVarDeclaration implements FStatement {

    private FVariable var;
    private FVarAssignment assignment; //optional

    public FVarDeclaration(FVariable var, FVarAssignment assignment) {
        this.var = var;
        this.assignment = assignment;
        assert assignment == null || var == assignment.getVariableExpression().getVariable();
    }

    public FVariable getVar() {
        return var;
    }

    public Optional<FVarAssignment> getAssignment() {
        return Optional.ofNullable(assignment);
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.enterVarDeclaration(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(var);
        getAssignment().ifPresent(a -> a.getValue().toString(sb.append(' ').append(a.getOperator()).append(' ')));
        return sb.append(';');
    }
    @Override
    public String toString() {
        return tS();
    }
}

package tys.frontier.code.statement;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.Optional;

public class FVarDeclaration implements FStatement {

    private FLocalVariable var;
    private FVarAssignment assignment; //optional TODO this is messy but works for now

    private FVarDeclaration(FLocalVariable var, FVarAssignment assignment) {
        this.var = var;
        this.assignment = assignment;
        assert assignment == null || var == assignment.getVariableExpression().getVariable();
    }

    public static FVarDeclaration create(FLocalVariable var, FVarAssignment assignment) {
        return new FVarDeclaration(var, assignment);
    }

    public static FVarDeclaration create(FLocalVariable var, FExpression initialValue) throws IncompatibleTypes {
        return new FVarDeclaration(var, FVarAssignment.create(new FLocalVariableExpression(var), FVarAssignment.Operator.ASSIGN, initialValue));
    }

    public static FVarDeclaration createTrusted(FLocalVariable var, FExpression initialValue) {
        try {
            return create(var, FVarAssignment.create(new FLocalVariableExpression(var), FVarAssignment.Operator.ASSIGN, initialValue));
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public FLocalVariable getVar() {
        return var;
    }

    public Optional<FVarAssignment> getAssignment() {
        return Optional.ofNullable(assignment);
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return Optional.empty();
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterVarDeclaration(this);
        return visitor.exitVarDeclaration(this, getAssignment().map(assignment -> assignment.getValue().accept(visitor)));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitVarDeclaration(this);
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

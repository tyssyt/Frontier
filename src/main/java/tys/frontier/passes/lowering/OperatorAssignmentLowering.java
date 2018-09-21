package tys.frontier.passes.lowering;

import tys.frontier.code.FFunction;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.*;
import tys.frontier.code.module.Module;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarAssignment;
import tys.frontier.code.statement.FVarDeclaration;
import tys.frontier.util.Utils;

import java.util.Arrays;

public class OperatorAssignmentLowering extends StatementReplacer {

    private static OperatorAssignmentLowering INSTANCE = new OperatorAssignmentLowering();

    private OperatorAssignmentLowering() {}

    public static void lower(Module module) {
        module.accept(INSTANCE);
    }


    @Override
    public FStatement exitVarAssignment(FVarAssignment assignment, FExpression variable, FExpression value) {
        return replace(assignment, currentFunction);
    }

    public FStatement replace (FVarAssignment assignment, FFunction function) { //TODO this code contains a lot of duplicates and introduce a stupid interface, but I was tired and it works...
        if (assignment.getOperator() == FVarAssignment.Operator.ASSIGN)
            return assignment;

        FVariableExpression varExp = assignment.getVariableExpression();
        if (varExp instanceof FLocalVariableExpression || varExp instanceof FFieldAccess && ((FFieldAccess) varExp).isStatic()) {
            FFunction op = assignment.getOperator().getOperator(varExp.getType());
            FVariableExpression load = varExp.copy();
            FExpression newValue = FFunctionCall.createStaticTrusted(op, Arrays.asList(load, assignment.getValue()));
            FVariableExpression store = varExp.copy();
            return FVarAssignment.createTrusted(store, FVarAssignment.Operator.ASSIGN, newValue);
        } else if (varExp instanceof HasInstanceObject) {
            HasInstanceObject oldExp = ((HasInstanceObject) varExp);
            FExpression container = oldExp.getObject();

            FLocalVariable containerVar = function.getFreshVariable(container.getType());
            FVarDeclaration containerDecl = FVarDeclaration.createTrusted(containerVar, container);

            FFunction op = assignment.getOperator().getOperator(varExp.getType());
            HasInstanceObject load = oldExp.copy();
            load.setObject(new FLocalVariableExpression(containerVar));
            FExpression newValue = FFunctionCall.createStaticTrusted(op, Arrays.asList(load, assignment.getValue()));
            HasInstanceObject store = oldExp.copy();
            store.setObject(new FLocalVariableExpression(containerVar));
            FVarAssignment res = FVarAssignment.createTrusted((FVariableExpression) store, FVarAssignment.Operator.ASSIGN, newValue);
            return FBlock.from(Arrays.asList(containerDecl, res));
        } else {
            return Utils.cantHappen();
        }
    }
}


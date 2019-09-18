package tys.frontier.passes.lowering;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.*;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarAssignment;
import tys.frontier.code.statement.FVarAssignment.Operator;
import tys.frontier.code.statement.FVarDeclaration;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class OperatorAssignmentLowering extends StatementReplacer {

    private static OperatorAssignmentLowering INSTANCE = new OperatorAssignmentLowering();

    private OperatorAssignmentLowering() {}

    public static void lower(Module module) {
        module.accept(INSTANCE);
    }

    @Override
    public FStatement exitVarAssignment(FVarAssignment assignment, List<FExpression> variables, List<FExpression> values) {
        return replace(assignment, currentFunction);
    }

    public FStatement replace (FVarAssignment assignment, FFunction function) { //TODO this code contains a lot of duplicates and introduce a stupid interface, but I was tired and it works...
        if (assignment.getOperator() == Operator.ASSIGN)
            return assignment;

        List<FStatement> res = new ArrayList<>();

        List<FExpression> unpackedValues;
        //if there is an expression of tuple type on the rhs, we need to unpack it by assigning it to temporary variables
        if (assignment.getVariables().size() != assignment.getValues().size()) {
            unpackedValues = new ArrayList<>(assignment.getVariables().size());

            List<FVariableExpression> temporaryVars = new ArrayList<>();
            List<FExpression> tupleExps = new ArrayList<>();

            //there is a function call that returns a tuple on the rhs
            //we first unpack the tuple by assigning it to temporary variables

            for (FExpression value : assignment.getValues()) {
                if (value.getType() instanceof FTuple) {
                    tupleExps.add(value);
                    for (FType type : ((FTuple) value.getType()).getTypes()) {
                        FLocalVariable v = function.getFreshVariable(type); //write rhs
                        temporaryVars.add(new FVarDeclaration(v)); //write lhs
                        unpackedValues.add(new FLocalVariableExpression(v)); //read
                    }
                } else {
                    unpackedValues.add(value);
                }
            }
            assert assignment.getVariables().size() == unpackedValues.size();

            //create the expression that assigns tuples to temporary variables
            res.add(FVarAssignment.createTrusted(temporaryVars, Operator.ASSIGN, tupleExps));

        } else {
            unpackedValues = assignment.getValues();
        }

        //on the Lhs we might have complex expressions like a.getB().c()[4] += 5
        //if we duplicate that expression to the rhs, unwanted side effects may occur and unneeded reads are done
        //we need to create a temporary local var t' := a.getB().c() so we can duplicate t' to both sides of the equation
        List<FVariableExpression> directAccessVars = new ArrayList<>(assignment.getVariables().size());
        List<FVariableExpression> newDirectAccessVars = new ArrayList<>();
        List<FExpression> newDirectAccessVarContainers = new ArrayList<>();
        for (FVariableExpression varExp : assignment.getVariables()) {
            if (varExp instanceof FLocalVariableExpression) {
                directAccessVars.add(new FLocalVariableExpression(((FLocalVariableExpression) varExp).getVariable()));
                continue;
            }
            assert varExp instanceof HasInstanceObject;
            HasInstanceObject oldExp = ((HasInstanceObject) varExp);
            if (varExp instanceof FFieldAccess && ((FFieldAccess) varExp).isStatic()) {
                directAccessVars.add(oldExp.copy()); //can use var directly
                continue;
            }

            FExpression container = oldExp.getObject();
            if (container instanceof FLocalVariableExpression) {
                directAccessVars.add(oldExp.copy()); //no need to store container in a local var if it already is one
                continue;
            }

            FLocalVariable containerVar = function.getFreshVariable(container.getType());
            newDirectAccessVars.add(new FVarDeclaration(containerVar));
            newDirectAccessVarContainers.add(container);

            HasInstanceObject directAccessVar = oldExp.copy();
            directAccessVar.setObject(new FLocalVariableExpression(containerVar));
            directAccessVars.add(directAccessVar);
        }
        if (!newDirectAccessVars.isEmpty()) {
            res.add(FVarAssignment.createTrusted(newDirectAccessVars, Operator.ASSIGN, newDirectAccessVarContainers));
        }

        //update rhs: v += e; => v = v+e;
        List<FExpression> newRhss = new ArrayList<>(unpackedValues.size());
        for (Pair<FVariableExpression, FExpression> pair : Utils.zip(directAccessVars, unpackedValues)) {
            FFunction op = assignment.getOperator().getOperator(((FClass) pair.a.getType()));
            newRhss.add(FFunctionCall.createTrusted(op, Arrays.asList(pair.a.copy(), pair.b)));
        }
        res.add(FVarAssignment.createTrusted(directAccessVars, Operator.ASSIGN, newRhss));
        return FBlock.from(res);
    }

}


package tys.frontier.passes.lowering;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.*;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.operator.FBinaryOperator;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarAssignment;
import tys.frontier.code.statement.FVarAssignment.Operator;
import tys.frontier.code.statement.loop.FForEach;
import tys.frontier.code.statement.loop.FWhile;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static java.util.Collections.singletonList;

public class FForEachLowering extends StatementReplacer {

    private static FForEachLowering INSTANCE = new FForEachLowering();

    private FForEachLowering() {}

    public static void lower(Module module) {
        module.accept(INSTANCE);
    }

    @Override
    public FStatement exitForEach(FForEach forEach, FExpression container, FStatement body) {
        return replace(forEach, currentFunction);
    }

    public FStatement replace (FForEach forEach, FFunction function) {
        if (!(forEach.getContainer().getType() instanceof FArray)) {
            Utils.NYI("non array for each");
        }
        FArray arrayType = ((FArray) forEach.getContainer().getType());
        List<FStatement> res = new ArrayList<>(4);

        //first store the array expression in a local variable, if necessary
        FLocalVariable container;
        {
            FExpression array = forEach.getContainer();
            if (array instanceof FLocalVariableExpression) {
                container = ((FLocalVariableExpression) array).getVariable();
            } else {
                container = function.getFreshVariable(arrayType);
                res.add(FVarAssignment.createDecl(container, array));
            }
        }

        //store the size in a local variable
        FLocalVariable size = function.getFreshVariable(FIntN._32);
        {
            FLocalVariableExpression containerAccess = new FLocalVariableExpression(container);
            FFieldAccess sizeAcc = FFieldAccess.createInstanceTrusted(arrayType.getInstanceFields().get(FArray.SIZE), containerAccess);
            res.add(FVarAssignment.createDecl(size, sizeAcc));
        }

        //declare counter
        FLocalVariable counter = function.getFreshVariable(FIntN._32);
        {
            res.add(FVarAssignment.createDecl(counter, new FLiteralExpression(new FIntNLiteral(0))));
        }

        //condition
        FExpression condition;
        {
            FFunction less = FBinaryOperator.Bool.LESS.getFunction(FIntN._32);
            condition = FFunctionCall.createTrusted(less, Arrays.asList(new FLocalVariableExpression(counter), new FLocalVariableExpression(size)));
        }

        //as first statement of loop accessing the array and storing the result in the iterator var
        FStatement itDecl;
        {
            FLocalVariable iterator = forEach.getIterator();
            FArrayAccess arrayAccess = FArrayAccess.createTrusted(new FLocalVariableExpression(container), new FLocalVariableExpression(counter));
            itDecl = FVarAssignment.createDecl(iterator, arrayAccess);
        }

        //increment
        FStatement increment;
        {
            FLocalVariableExpression counterExp = new FLocalVariableExpression(counter);
            FLiteralExpression one = new FLiteralExpression(new FIntNLiteral(1));
            increment = FVarAssignment.createTrusted(singletonList(counterExp), Operator.ADD_ASSIGN, Arrays.asList(one));
        }

        //Loop Body
        FBlock loopBody = FBlock.from(itDecl, forEach.getBody(), increment);

        //While Loop
        FWhile fWhile = FWhile.createTrusted(forEach.getNestedDepth(), forEach.getIdentifier(), condition, loopBody);
        forEach.getIdentifier().setLoop(fWhile);
        res.add(fWhile);

        return FBlock.from(res);
    }


}

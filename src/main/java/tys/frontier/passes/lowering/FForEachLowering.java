package tys.frontier.passes.lowering;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.*;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.FForEach;
import tys.frontier.code.statement.loop.FWhile;
import tys.frontier.code.type.FClass;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
                res.add(FAssignment.createDecl(container, array));
            }
        }

        //store the size in a local variable
        FLocalVariable size = function.getFreshVariable(FIntN._32);
        {
            FLocalVariableExpression containerAccess = new FLocalVariableExpression(container);
            FFieldAccess sizeAcc = FFieldAccess.createInstanceTrusted(arrayType.getInstanceFields().get(FArray.SIZE), containerAccess);
            res.add(FAssignment.createDecl(size, sizeAcc));
        }

        //declare counter
        FLocalVariable counter = forEach.getCounter().orElse(function.getFreshVariable(FIntN._32));
        {
            res.add(FAssignment.createDecl(counter, new FLiteralExpression(new FIntNLiteral(0))));
        }

        //condition
        FExpression condition;
        {
            Signature less = BinaryOperator.LESS.getFunction(FIntN._32).getSignature();
            condition = FFunctionCall.createTrusted(less, Arrays.asList(new FLocalVariableExpression(counter), new FLocalVariableExpression(size)));
        }

        //as first statement of loop accessing the array and storing the result in the iterator var
        FStatement itDecl;
        {
            List<FExpression> decls = new ArrayList<>(forEach.getIterators().size());
            for (FLocalVariable it : forEach.getIterators()) {
                decls.add(new FVarDeclaration(it));
            }
            FArrayAccess arrayAccess = FArrayAccess.createTrusted(new FLocalVariableExpression(container), new FLocalVariableExpression(counter));
            itDecl = FAssignment.createTrusted(decls, Arrays.asList(arrayAccess));
        }

        //increment
        FStatement increment;
        {
            FLocalVariableExpression counterExp = new FLocalVariableExpression(counter);
            Signature inc = UnaryOperator.INC.getFunction((FClass) counter.getType()).getSignature();
            increment = new FExpressionStatement(FFunctionCall.createTrusted(inc, Arrays.asList(counterExp)));
            counterExp.setAccessType(FVariableExpression.AccessType.LOAD_AND_STORE);
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

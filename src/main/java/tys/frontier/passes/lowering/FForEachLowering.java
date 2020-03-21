package tys.frontier.passes.lowering;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.statement.FAssignment;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarDeclaration;
import tys.frontier.code.statement.loop.FForEach;
import tys.frontier.code.statement.loop.FWhile;
import tys.frontier.code.statement.loop.forImpl.ForByIdx;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.type.FClass;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static tys.frontier.util.Utils.mutableSingletonList;

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
        ForImpl forImpl = forEach.getContainer().getType().getForImpl();
        if (forImpl instanceof ForByIdx)
            return buildForByIdx((ForByIdx) forImpl, forEach, function);
        else
            return Utils.NYI("ForImpl: " + forImpl);

    }

    private FStatement buildForByIdx(ForByIdx forImpl, FForEach forEach, FFunction function) {
        List<FStatement> res = new ArrayList<>(4);

        //first store the container expression in a local variable, if necessary
        FLocalVariable container;
        {
            FExpression containerExpression = forEach.getContainer();
            if (containerExpression instanceof FLocalVariableExpression) {
                container = ((FLocalVariableExpression) containerExpression).getVariable();
            } else {
                container = function.getFreshVariable(forEach.getContainer().getType());
                res.add(FAssignment.createDecl(container, containerExpression));
            }
        }

        //store the size in a local variable
        FLocalVariable size = function.getFreshVariable(FIntN._32);
        {
            FLocalVariableExpression containerAccess = new FLocalVariableExpression(container);
            FFunctionCall sizeGetterFc = FFunctionCall.createTrusted(forImpl.getGetSize().getSignature(), mutableSingletonList(containerAccess));
            res.add(FAssignment.createDecl(size, sizeGetterFc));
        }

        //declare counter
        FLocalVariable counter = forEach.getCounter().orElse(function.getFreshVariable(FIntN._32));
        res.add(FAssignment.createDecl(counter, new FLiteralExpression(new FIntNLiteral(0))));

        //condition
        FExpression condition;
        {
            Signature less = BinaryOperator.LESS.getFunction(FIntN._32.getNamespace()).getSignature();
            condition = FFunctionCall.createTrusted(less, asList(new FLocalVariableExpression(counter), new FLocalVariableExpression(size)));
        }

        //as first statement of loop accessing the array and storing the result in the iterator var
        FStatement itDecl;
        {
            List<FExpression> decls = new ArrayList<>(forEach.getIterators().size());
            for (FLocalVariable it : forEach.getIterators()) {
                decls.add(new FVarDeclaration(it));
            }
            List<FExpression> arguments = asList(new FLocalVariableExpression(container), new FLocalVariableExpression(counter));
            FFunctionCall arrayAccess = FFunctionCall.createTrusted(forImpl.getGetElement().getSignature(), arguments);
            itDecl = FAssignment.createTrusted(decls, mutableSingletonList(arrayAccess));
        }

        //increment
        FStatement increment;
        {
            FLocalVariableExpression rhsCounterExp = new FLocalVariableExpression(counter);
            FLiteralExpression one = new FLiteralExpression(new FIntNLiteral(1));
            FFunction plus = BinaryOperator.PLUS.getFunction(((FClass) counter.getType()).getNamespace());
            FFunctionCall plusCall = FFunctionCall.createTrusted(plus.getSignature(), asList(rhsCounterExp, one));

            FLocalVariableExpression lhsCounterExp = new FLocalVariableExpression(counter);
            increment = FAssignment.createTrusted(singletonList(lhsCounterExp), mutableSingletonList(plusCall));
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

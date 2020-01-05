package tys.frontier.passes.lowering;

import com.google.common.collect.ImmutableListMultimap;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FieldAccessor;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.statement.FAssignment;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarDeclaration;
import tys.frontier.code.statement.loop.FForEach;
import tys.frontier.code.statement.loop.FWhile;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static java.util.Collections.singletonList;
import static tys.frontier.util.Utils.typesFromExpressionList;

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
            FieldAccessor sizeGetter = arrayType.getInstanceFields().get(FArray.SIZE).getGetter();
            FFunctionCall sizeGetterFc = FFunctionCall.createTrusted(sizeGetter.getSignature(), Arrays.asList(containerAccess));
            res.add(FAssignment.createDecl(size, sizeGetterFc));
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
            try {
                List<FExpression> arguments = Arrays.asList(new FLocalVariableExpression(container), new FLocalVariableExpression(counter));
                FunctionResolver.Result result = container.getType().hardResolveFunction(Access.ID, typesFromExpressionList(arguments), ImmutableListMultimap.of(), null, false);
                FFunctionCall arrayAccess = FFunctionCall.createTrusted(result.signature, arguments);
                itDecl = FAssignment.createTrusted(decls, Arrays.asList(arrayAccess));
            } catch (FunctionNotFound functionNotFound) {
                return Utils.cantHappen();
            }
        }

        //increment
        FStatement increment;
        {
            FLocalVariableExpression rhsCounterExp = new FLocalVariableExpression(counter);
            FLiteralExpression one = new FLiteralExpression(new FIntNLiteral(1));
            FFunction plus = BinaryOperator.PLUS.getFunction((FClass) counter.getType());
            FFunctionCall plusCall = FFunctionCall.createTrusted(plus.getSignature(), Arrays.asList(rhsCounterExp, one));

            FLocalVariableExpression lhsCounterExp = new FLocalVariableExpression(counter);
            increment = FAssignment.createTrusted(singletonList(lhsCounterExp), Arrays.asList(plusCall));
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

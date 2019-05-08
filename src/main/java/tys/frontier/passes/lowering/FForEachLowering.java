package tys.frontier.passes.lowering;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.*;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.operator.FBinaryOperator;
import tys.frontier.code.function.operator.FUnaryOperator;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarDeclaration;
import tys.frontier.code.statement.loop.FFor;
import tys.frontier.code.statement.loop.FForEach;
import tys.frontier.util.Utils;

import java.util.Arrays;

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

        //first store the array expression in a fresh variable, if necessary
        FExpression array = forEach.getContainer();
        FArray arrayType = ((FArray) array.getType());
        FLocalVariable arrayVar;
        FVarDeclaration arrayDecl = null;
        if (array instanceof FLocalVariableExpression) {
            arrayVar = ((FLocalVariableExpression) array).getVariable();
        } else {
            arrayVar = function.getFreshVariable(arrayType);
            arrayDecl = FVarDeclaration.createTrusted(arrayVar, array);
        }

        //counter declatation
        FLocalVariable counter = function.getFreshVariable(FIntN._32);
        FLiteralExpression zero = new FLiteralExpression(new FIntNLiteral(0));
        FVarDeclaration decl = FVarDeclaration.createTrusted(counter, zero);

        //condition
        FFunction less = FBinaryOperator.Bool.LESS.getFunction(FIntN._32);
        FFieldAccess size = FFieldAccess.createInstanceTrusted(arrayType.getInstanceFields().get(FArray.SIZE), array);
        FExpression cond = FFunctionCall.createTrusted(less, Arrays.asList(new FLocalVariableExpression(counter), size));

        //increment
        FFunction inc = FUnaryOperator.Pre.INC.getFunction(FIntN._32);
        FLocalVariableExpression invVarExpre = new FLocalVariableExpression(counter);
        invVarExpre.setAccessType(FVariableExpression.AccessType.LOAD_AND_STORE); //TODO setting this here explicitly is ugly, but fine for now
        FExpression incCall = FFunctionCall.createTrusted(inc, Arrays.asList(invVarExpre));

        //as first statement of loop accessing the array and storing the result in the iterator var
        FLocalVariable iterator = forEach.getIterator();
        FArrayAccess arrayAccess = FArrayAccess.createTrusted(new FLocalVariableExpression(arrayVar), new FLocalVariableExpression(counter));
        FVarDeclaration itDecl = FVarDeclaration.createTrusted(iterator, arrayAccess);

        FBlock body = FBlock.from(itDecl, forEach.getBody());

        //create for and update loop identifier
        FFor ffor = FFor.createTrusted(forEach.getNestedDepth(), forEach.getIdentifier(), decl, cond, incCall, body);
        forEach.getIdentifier().setLoop(ffor);

        if (arrayDecl == null)
            return ffor;
        else
            return FBlock.from(arrayDecl, ffor);
    }


}

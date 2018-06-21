package tys.frontier.passes.lowering;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FFile;
import tys.frontier.code.FFunction;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.Operator.FUnaryOperator;
import tys.frontier.code.expression.*;
import tys.frontier.code.literal.FIntNLiteral;
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

    public static void lower(FFile file) {
        file.accept(INSTANCE);
    }

    @Override
    public FStatement exitForEach(FForEach forEach, FExpression container, FStatement body) {
        return replace(forEach, currentFunction);
    }

    public FStatement replace (FForEach forEach, FFunction function) {
        if (!(forEach.getContainer().getType() instanceof FArray)) {
            Utils.NYI("non array for each");
        }

        //first store the array expression in a fresh variable
        FExpression array = forEach.getContainer();
        FLocalVariable arrayVar = function.getFreshVariable(array.getType());
        FVarDeclaration arrayDecl = new FVarDeclaration(arrayVar, array);

        //counter declatation
        FLocalVariable counter = function.getFreshVariable(FIntN._32);
        FLiteralExpression zero = new FLiteralExpression(new FIntNLiteral(0, 32));
        FVarDeclaration decl = new FVarDeclaration(counter, zero);

        //condition
        FFunction less = Iterables.getOnlyElement(FIntN._32.getFunctions(FBinaryOperator.Bool.LESS.identifier));
        FFieldAccess size = new FFieldAccess(array.getType().getField(FArray.SIZE), array);
        FExpression cond = new FFunctionCall(less, ImmutableList.of(new FLocalVariableExpression(counter), size));

        //increment
        FFunction inc = Iterables.getOnlyElement(FIntN._32.getFunctions(FUnaryOperator.Pre.INC.identifier));
        FLocalVariableExpression invVarExpre = new FLocalVariableExpression(counter);
        invVarExpre.setAccessType(FVariableExpression.AccessType.LOAD_AND_STORE); //TODO setting this here explicitly is ugly, but fine for now
        FExpression incCall = new FFunctionCall(invVarExpre, inc, ImmutableList.of());

        //as first statement of loop accessing the array and storing the result in the iterator var
        FLocalVariable iterator = forEach.getIterator();
        FArrayAccess arrayAccess = new FArrayAccess(new FLocalVariableExpression(arrayVar), new FLocalVariableExpression(counter));
        FVarDeclaration itDecl = new FVarDeclaration(iterator, arrayAccess);

        FBlock body = FBlock.from(Arrays.asList(itDecl, forEach.getBody()));

        //create for and update loop identifier
        FFor ffor = new FFor(forEach.getNestedDepth(), forEach.getIdentifier(), decl, cond, incCall, body);
        forEach.getIdentifier().setLoop(ffor);

        return FBlock.from(Arrays.asList(arrayDecl, ffor));
    }


}
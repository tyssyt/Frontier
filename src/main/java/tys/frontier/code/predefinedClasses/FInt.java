package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FClassIdentifier;

import static tys.frontier.code.identifier.FFunctionIdentifier.*;

public class FInt extends FPredefinedClass {

    public static final FInt INSTANCE = new FInt();

    private FInt () {
        super(FClassIdentifier.INT);

        addFunctionInternal(new FPredefinedOperator.Unary(PLUS, this));
        addFunctionInternal(new FPredefinedOperator.Unary(MINUS, this));

        addFunctionInternal(new FPredefinedOperator.Binary(PLUS, this));
        addFunctionInternal(new FPredefinedOperator.Binary(MINUS, this));
        addFunctionInternal(new FPredefinedOperator.Binary(TIMES, this));
        addFunctionInternal(new FPredefinedOperator.Binary(DIVIDED, this));
        addFunctionInternal(new FPredefinedOperator.Binary(MODULO, this));

        addFunctionInternal(new FPredefinedOperator.Binary(SMALLER, this, this, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(GREATER, this, this, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(SMALLER_EQUAL, this, this, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(GREATER_EQUAL, this, this, FBool.INSTANCE, false));
    }

}

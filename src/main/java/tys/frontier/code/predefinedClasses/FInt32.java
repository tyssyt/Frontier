package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FClassIdentifier;

import static tys.frontier.code.identifier.FFunctionIdentifier.*;

public class FInt32 extends FPredefinedClass {

    public static final FInt32 INSTANCE = new FInt32();

    private FInt32 () {
        super(FClassIdentifier.INT32);

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

        addFunctionInternal(new FPredefinedOperator.Binary(PLUS, this, FInt.INSTANCE, FInt.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(PLUS, this, FInt.INSTANCE, FInt.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(MINUS, this, FInt.INSTANCE, FInt.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(MINUS, this, FInt.INSTANCE, FInt.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(TIMES, this, FInt.INSTANCE, FInt.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(TIMES, this, FInt.INSTANCE, FInt.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(DIVIDED, this, FInt.INSTANCE, FInt.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(DIVIDED, this, FInt.INSTANCE, FInt.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(MODULO, this, FInt.INSTANCE, FInt.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(MODULO, this, FInt.INSTANCE, FInt.INSTANCE, true));

        addFunctionInternal(new FPredefinedOperator.Binary(SMALLER, this, FInt.INSTANCE, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(SMALLER, this, FInt.INSTANCE, FBool.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(GREATER, this, FInt.INSTANCE, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(GREATER, this, FInt.INSTANCE, FBool.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(SMALLER_EQUAL, this, FInt.INSTANCE, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(SMALLER_EQUAL, this, FInt.INSTANCE, FBool.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(GREATER_EQUAL, this, FInt.INSTANCE, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(GREATER_EQUAL, this, FInt.INSTANCE, FBool.INSTANCE, true));

        addFunctionInternal(new FPredefinedOperator.Binary(PLUS, this, FInt64.INSTANCE, FInt64.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(PLUS, this, FInt64.INSTANCE, FInt64.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(MINUS, this, FInt64.INSTANCE, FInt64.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(MINUS, this, FInt64.INSTANCE, FInt64.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(TIMES, this, FInt64.INSTANCE, FInt64.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(TIMES, this, FInt64.INSTANCE, FInt64.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(DIVIDED, this, FInt64.INSTANCE, FInt64.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(DIVIDED, this, FInt64.INSTANCE, FInt64.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(MODULO, this, FInt64.INSTANCE, FInt64.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(MODULO, this, FInt64.INSTANCE, FInt64.INSTANCE, true));

        addFunctionInternal(new FPredefinedOperator.Binary(SMALLER, this, FInt64.INSTANCE, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(SMALLER, this, FInt64.INSTANCE, FBool.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(GREATER, this, FInt64.INSTANCE, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(GREATER, this, FInt64.INSTANCE, FBool.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(SMALLER_EQUAL, this, FInt64.INSTANCE, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(SMALLER_EQUAL, this, FInt64.INSTANCE, FBool.INSTANCE, true));
        addFunctionInternal(new FPredefinedOperator.Binary(GREATER_EQUAL, this, FInt64.INSTANCE, FBool.INSTANCE, false));
        addFunctionInternal(new FPredefinedOperator.Binary(GREATER_EQUAL, this, FInt64.INSTANCE, FBool.INSTANCE, true));
    }

}

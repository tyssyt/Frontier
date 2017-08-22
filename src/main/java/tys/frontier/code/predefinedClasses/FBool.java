package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FClassIdentifier;

import static tys.frontier.code.identifier.FFunctionIdentifier.*;

public class FBool extends FPredefinedClass {

    public static final FBool INSTANCE = new FBool();

    private FBool() {
        super(FClassIdentifier.BOOL);

        addFunctionInternal(new FPredefinedOperator.Unary(NOT, this));
        addFunctionInternal(new FPredefinedOperator.Binary(AND, this));
        addFunctionInternal(new FPredefinedOperator.Binary(OR, this));
        addFunctionInternal(new FPredefinedOperator.Binary(XOR, this));

    }
}

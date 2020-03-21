package tys.frontier.code.predefinedClasses;

import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;

public class FBool extends FPredefinedClass {

    public static final FBool INSTANCE;

    static { //adding the functions needs the INSTANCE field to be initialised, so we ensure proper order of instructions here
        INSTANCE = new FBool();
        INSTANCE.addPredefinedFunctionsForBoolType();
    }


    private FBool() {
        super(FIdentifier.BOOL);
    }

    private void addPredefinedFunctionsForBoolType() {
        DefaultNamespace namespace = getNamespace();
        namespace.addFunctionTrusted(UnaryOperator.NOT.createPredefined(this, this));
        namespace.addFunctionTrusted(BinaryOperator.EQUALS_ID.createPredefined(this, this, this));
        namespace.addFunctionTrusted(BinaryOperator.NOT_EQUALS_ID.createPredefined(this, this, this));
        namespace.addFunctionTrusted(BinaryOperator.EQUALS.createPredefined(this, this, this));
        namespace.addFunctionTrusted(BinaryOperator.NOT_EQUALS.createPredefined(this, this, this));
        namespace.addFunctionTrusted(BinaryOperator.AND.createPredefined(this, this, this));
        namespace.addFunctionTrusted(BinaryOperator.OR.createPredefined(this, this, this));
        namespace.addFunctionTrusted(BinaryOperator.AAND.createPredefined(this, this, this));
        namespace.addFunctionTrusted(BinaryOperator.AOR.createPredefined(this, this, this));
        namespace.addFunctionTrusted(BinaryOperator.XOR.createPredefined(this, this, this));
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }
}

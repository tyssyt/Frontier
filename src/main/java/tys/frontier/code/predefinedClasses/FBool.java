package tys.frontier.code.predefinedClasses;

import tys.frontier.code.function.operator.FBinaryOperator;
import tys.frontier.code.function.operator.FUnaryOperator;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

public class FBool extends FPredefinedClass {

    public static final FBool INSTANCE;

    static { //adding the functions needs the INSTANCE field to be initialised, so we ensure proper order of instructions here
        INSTANCE = new FBool();
        INSTANCE.addFunctionsInstance();
    }


    private FBool() {
        super(FTypeIdentifier.BOOL);
    }

    private void addFunctionsInstance() {
        try {
            addFunction(FUnaryOperator.Pre.NOT.createPredefined(this));
            addFunction(FBinaryOperator.Bool.EQUALS_ID.createPredefined(this));
            addFunction(FBinaryOperator.Bool.NOT_EQUALS_ID.createPredefined(this));
            addFunction(FBinaryOperator.Bool.EQUALS.createPredefined(this));
            addFunction(FBinaryOperator.Bool.NOT_EQUALS.createPredefined(this));
            addFunction(FBinaryOperator.Bool.AND.createPredefined(this));
            addFunction(FBinaryOperator.Bool.OR.createPredefined(this));
            addFunction(FBinaryOperator.Arith.AND.createPredefined(this));
            addFunction(FBinaryOperator.Arith.OR.createPredefined(this));
            addFunction(FBinaryOperator.Arith.XOR.createPredefined(this));
        } catch (SignatureCollision signatureCollision) {
            Utils.handleException(signatureCollision);
        }
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }
}

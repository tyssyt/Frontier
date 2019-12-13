package tys.frontier.code.predefinedClasses;

import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
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
            addFunction(UnaryOperator.NOT.createPredefined(this, this));
            addFunction(BinaryOperator.EQUALS_ID.createPredefined(this, this, this));
            addFunction(BinaryOperator.NOT_EQUALS_ID.createPredefined(this, this, this));
            addFunction(BinaryOperator.EQUALS.createPredefined(this, this, this));
            addFunction(BinaryOperator.NOT_EQUALS.createPredefined(this, this, this));
            addFunction(BinaryOperator.AND.createPredefined(this, this, this));
            addFunction(BinaryOperator.OR.createPredefined(this, this, this));
            addFunction(BinaryOperator.AAND.createPredefined(this, this, this));
            addFunction(BinaryOperator.AOR.createPredefined(this, this, this));
            addFunction(BinaryOperator.XOR.createPredefined(this, this, this));
        } catch (SignatureCollision signatureCollision) {
            Utils.handleException(signatureCollision);
        }
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }
}

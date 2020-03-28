package tys.frontier.code.predefinedClasses;

import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

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

        try {
            namespace.addRemoteFunction(BinaryOperator.EQUALS_ID.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.NOT_EQUALS_ID.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.EQUALS.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.NOT_EQUALS.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.AND.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.OR.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.AAND.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.AOR.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.XOR.addPredefined(this, this));
        } catch (SignatureCollision signatureCollision) {
            Utils.cantHappen();
        }
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }
}

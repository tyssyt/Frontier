package tys.frontier.code.predefinedClasses;

import tys.frontier.code.function.FunctionBuilder;
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
        namespace.addFunctionTrusted(new FunctionBuilder(UnaryOperator.NOT.identifier, namespace)
                .setPredefined(true).setParams(this).setReturnType(this).build());

        try {
            BinaryOperator.EQUALS_ID.addPredefined(this, this);
            BinaryOperator.NOT_EQUALS_ID.addPredefined(this, this);
            BinaryOperator.EQUALS.addPredefined(this, this);
            BinaryOperator.NOT_EQUALS.addPredefined(this, this);
            BinaryOperator.AND.addPredefined(this, this);
            BinaryOperator.OR.addPredefined(this, this);
            BinaryOperator.AAND.addPredefined(this, this);
            BinaryOperator.AOR.addPredefined(this, this);
            BinaryOperator.XOR.addPredefined(this, this);
        } catch (SignatureCollision signatureCollision) {
            Utils.cantHappen();
        }
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }
}

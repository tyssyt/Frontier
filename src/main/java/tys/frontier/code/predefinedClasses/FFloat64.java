package tys.frontier.code.predefinedClasses;

import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.Operator.FUnaryOperator;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

public class FFloat64 extends FPredefinedClass {

    public static final FFloat64 INSTANCE = new FFloat64();

    private FFloat64 () {
        super(FTypeIdentifier.FLOAT64);

        try {
            addFunction(FUnaryOperator.Pre.NEG.createPredefined(this));

            addFunction(FBinaryOperator.Bool.EQUALS.createPredefined(this));
            addFunction(FBinaryOperator.Bool.NOT_EQUALS.createPredefined(this));
            addFunction(FBinaryOperator.Bool.EQUALS_ID.createPredefined(this));
            addFunction(FBinaryOperator.Bool.NOT_EQUALS_ID.createPredefined(this));
            addFunction(FBinaryOperator.Bool.LESS.createPredefined(this));
            addFunction(FBinaryOperator.Bool.LESS_EQUAL.createPredefined(this));
            addFunction(FBinaryOperator.Bool.GREATER.createPredefined(this));
            addFunction(FBinaryOperator.Bool.GREATER_EQUAL.createPredefined(this));

            addFunction(FBinaryOperator.Arith.PLUS.createPredefined(this));
            addFunction(FBinaryOperator.Arith.MINUS.createPredefined(this));
            addFunction(FBinaryOperator.Arith.TIMES.createPredefined(this));
            addFunction(FBinaryOperator.Arith.DIVIDED.createPredefined(this));
            addFunction(FBinaryOperator.Arith.MODULO.createPredefined(this));
        } catch (SignatureCollision signatureCollision) {
            Utils.handleException(signatureCollision);
        }
    }
}

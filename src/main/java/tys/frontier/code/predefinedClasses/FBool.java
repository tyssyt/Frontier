package tys.frontier.code.predefinedClasses;

import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.Operator.FUnaryOperator;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.literal.FBoolLiteral;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

public class FBool extends FPredefinedClass {

    public static final FBool INSTANCE = new FBool();

    public static final FFunctionIdentifier NOT = new FFunctionIdentifier("!");

    private FBool() {
        super(FClassIdentifier.BOOL);
        addFunctionsInstance();
    }

    @Override
    public FExpression getDefaultValue() {
        return new FLiteralExpression(FBoolLiteral.FALSE);
    }

    public static void addFunctions() {
        INSTANCE.addFunctionsInstance();
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
}

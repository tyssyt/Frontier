package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.FAssignment;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.type.FType;

import java.util.Arrays;
import java.util.List;

import static tys.frontier.util.Utils.mutableSingletonList;

public class FFloat32 extends FFloat {

    public static final FFloat32 INSTANCE = new FFloat32();

    @Override
    public boolean canImplicitlyCast() {
        return true; //to float64
    }

    private FFloat32 () {
        super(FIdentifier.FLOAT32);
        addPredefinedFunctionsForArithType();

        DefaultNamespace namespace = this.getNamespace();

        FunctionBuilder builder = new FunctionBuilder().setMemberOf(getNamespace()).setPredefined(true).setParams(this);
        FBaseFunction rawBits = builder.setIdentifier(RAW_BITS).setReturnType(FIntN._32).build();
        namespace.addFunctionTrusted(rawBits);
        builder.setReturnType(this);
        namespace.addFunctionTrusted(builder.setIdentifier(LOG).build());
        namespace.addFunctionTrusted(builder.setIdentifier(LOG10).build());
        namespace.addFunctionTrusted(builder.setIdentifier(LOG2).build());
        namespace.addFunctionTrusted(builder.setIdentifier(CEIL).build());
        namespace.addFunctionTrusted(builder.setIdentifier(FLOOR).build());
        namespace.addFunctionTrusted(builder.setIdentifier(TRUNC).build());

        builder.setPredefined(false);

        //TODO @PositionForGeneratedCode
        {
            FType returnType = FTuple.from(FBool.INSTANCE, FIntN._32, FIntN._32); //TODO how about int 24 & 7?
            FBaseFunction splitRepresentation = builder.setIdentifier(SPLIT_REPRESENTATION).setReturnType(returnType).build();
            namespace.addFunctionTrusted(splitRepresentation);

            FLocalVariable bits = new FLocalVariable(new FIdentifier("bits"), FIntN._32);
            FParameter firstParam = splitRepresentation.getSignature().getParameters().get(0);
            List<FExpression> arguments = mutableSingletonList(new FVariableExpression(null, firstParam));
            FAssignment bitsDecl = FAssignment.createDecl(bits, FFunctionCall.createTrusted(null, rawBits.getSignature(), arguments));

            //sign: bits < 0
            Signature less = BinaryOperator.LESS.getFunctionTrusted(FIntN._32, FIntN._32);
            arguments = Arrays.asList(new FVariableExpression(null, bits), new FLiteralExpression(null, new FIntNLiteral(0)));
            FExpression sign = FFunctionCall.createTrusted(null, less, arguments);

            //exponent: (bits >> 23) & 0xFF
            Signature uShiftR = FIntN._32.getUShiftR().getSignature();
            Signature aAnd = BinaryOperator.AAND.getFunctionTrusted(FIntN._32, FIntN._32);

            arguments = Arrays.asList(new FVariableExpression(null, bits), new FLiteralExpression(null, new FIntNLiteral(23)));
            FFunctionCall expShift = FFunctionCall.createTrusted(null, uShiftR, arguments);
            arguments = Arrays.asList(expShift, new FLiteralExpression(null, new FIntNLiteral(0xFF)));
            FExpression exponent = FFunctionCall.createTrusted(null, aAnd, arguments);

            //mantissa: bits & 0x7FFFFF
            arguments = Arrays.asList(new FVariableExpression(null, bits), new FLiteralExpression(null, new FIntNLiteral(0x7FFFFF)));
            FExpression mantissa = FFunctionCall.createTrusted(null, aAnd, arguments);

            FReturn _return = FReturn.createTrusted(null, List.of(sign, exponent, mantissa), splitRepresentation);
            splitRepresentation.setBody(FBlock.from(null, bitsDecl, _return));
        }
    }

    @Override
    public int getBits() {
        return 32;
    }
}

package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.FLocalVariableExpression;
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

public class FFloat32 extends FPredefinedClass {

    public static FIdentifier RAW_BITS = new FIdentifier("asRawBits");
    public static FIdentifier SPLIT_REPRESENTATION = new FIdentifier("splitRepresentation");

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

        builder.setPredefined(false);

        {
            FType returnType = FTuple.from(FBool.INSTANCE, FIntN._32, FIntN._32); //TODO how about int 24 & 7?
            FBaseFunction splitRepresentation = builder.setIdentifier(SPLIT_REPRESENTATION).setReturnType(returnType).build();
            namespace.addFunctionTrusted(splitRepresentation);

            FLocalVariable bits = new FLocalVariable(new FIdentifier("bits"), FIntN._32);
            FParameter firstParam = splitRepresentation.getSignature().getParameters().get(0);
            List<FExpression> arguments = mutableSingletonList(new FLocalVariableExpression(firstParam));
            FAssignment bitsDecl = FAssignment.createDecl(bits, FFunctionCall.createTrusted(rawBits.getSignature(), arguments));

            //sign: bits < 0
            Signature less = BinaryOperator.LESS.getFunctionTrusted(FIntN._32, FIntN._32);
            arguments = Arrays.asList(new FLocalVariableExpression(bits), new FLiteralExpression(new FIntNLiteral(0)));
            FExpression sign = FFunctionCall.createTrusted(less, arguments);

            //exponent: (bits >> 23) & 0xFF
            Signature uShiftR = FIntN._32.getUShiftR().getSignature();
            Signature aAnd = BinaryOperator.AAND.getFunctionTrusted(FIntN._32, FIntN._32);

            arguments = Arrays.asList(new FLocalVariableExpression(bits), new FLiteralExpression(new FIntNLiteral(23)));
            FFunctionCall expShift = FFunctionCall.createTrusted(uShiftR, arguments);
            arguments = Arrays.asList(expShift, new FLiteralExpression(new FIntNLiteral(0xFF)));
            FExpression exponent = FFunctionCall.createTrusted(aAnd, arguments);

            //mantissa: bits & 0x7FFFFF
            arguments = Arrays.asList(new FLocalVariableExpression(bits), new FLiteralExpression(new FIntNLiteral(0x7FFFFF)));
            FExpression mantissa = FFunctionCall.createTrusted(aAnd, arguments);

            FReturn _return = FReturn.createTrusted(List.of(sign, exponent, mantissa), splitRepresentation);
            splitRepresentation.setBody(FBlock.from(bitsDecl, _return));
        }
    }
}

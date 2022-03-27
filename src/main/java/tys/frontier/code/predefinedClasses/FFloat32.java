package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.StaticField;
import tys.frontier.code.expression.*;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FFloat32Literal;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.FAssignment;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.type.FType;

import java.util.List;

import static java.util.Arrays.asList;
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
    }

    @Override
    public int getBits() {
        return 32;
    }
    
    @SuppressWarnings("SameParameterValue")
    void addIntFunctions(FIntN int32, FIntN int64) { //needed to avoid cyclic class loader dependencies
        DefaultNamespace namespace = this.getNamespace();

        {
            StaticField nan = new StaticField(null, NAN, this, namespace, FVisibilityModifier.EXPORT, true);
            nan.setAssignmentTrusted(new FLiteralExpression(null, new FFloat32Literal(Float.NaN)));
            namespace.addFieldTrusted(nan);

            StaticField infPos = new StaticField(null, INF_POS, this, namespace, FVisibilityModifier.EXPORT, true);
            infPos.setAssignmentTrusted(new FLiteralExpression(null, new FFloat32Literal(Float.POSITIVE_INFINITY)));
            namespace.addFieldTrusted(infPos);

            StaticField infNeg = new StaticField(null, INF_NEG, this, namespace, FVisibilityModifier.EXPORT, true);
            infNeg.setAssignmentTrusted(new FLiteralExpression(null, new FFloat32Literal(Float.NEGATIVE_INFINITY)));
            namespace.addFieldTrusted(infNeg);
        }

        FunctionBuilder builder = new FunctionBuilder().setMemberOf(getNamespace()).setPredefined(true).setParams(this);
        FBaseFunction rawBits = builder.setIdentifier(RAW_BITS).setReturnType(int32).build();
        namespace.addFunctionTrusted(rawBits);

        builder.setReturnType(this);
        namespace.addFunctionTrusted(builder.setIdentifier(LOG).build());
        namespace.addFunctionTrusted(builder.setIdentifier(LOG10).build());
        namespace.addFunctionTrusted(builder.setIdentifier(LOG2).build());
        namespace.addFunctionTrusted(builder.setIdentifier(CEIL).build());
        namespace.addFunctionTrusted(builder.setIdentifier(FLOOR).build());
        namespace.addFunctionTrusted(builder.setIdentifier(TRUNC).build());
        namespace.addFunctionTrusted(builder.setIdentifier(SIN).build());
        namespace.addFunctionTrusted(builder.setIdentifier(COS).build());

        namespace.addFunctionTrusted(builder.setIdentifier(TO_INT32).setReturnType(int32).build());
        namespace.addFunctionTrusted(builder.setIdentifier(TO_INT64).setReturnType(int64).build());

        builder.setPredefined(false);

        { //TODO @PositionForGeneratedCode
            FType returnType = FTuple.from(FBool.INSTANCE, int32, int32); //TODO how about int 24 & 7?
            FBaseFunction splitRepresentation = builder.setIdentifier(SPLIT_REPRESENTATION).setReturnType(returnType).build();
            namespace.addFunctionTrusted(splitRepresentation);

            FLocalVariable bits = new FLocalVariable(null, new FIdentifier("bits"), int32);
            FParameter firstParam = splitRepresentation.getSignature().getParameters().get(0);
            List<FExpression> arguments = mutableSingletonList(new FVariableExpression(null, firstParam));
            FAssignment bitsDecl = FAssignment.createDecl(bits, FFunctionCall.create(null, rawBits.getSignature(), arguments));

            //sign: bits < 0
            Signature less = BinaryOperator.LESS.getFunctionTrusted(int32, int32);
            arguments = asList(new FVariableExpression(null, bits), new FLiteralExpression(null, new FIntNLiteral(0, int32)));
            FExpression sign = FFunctionCall.create(null, less, arguments);

            //exponent: (bits >> 23) & 0xFF
            Signature uShiftR = int32.getUShiftR().getSignature();
            Signature aAnd = BinaryOperator.AAND.getFunctionTrusted(int32, int32);

            arguments = asList(new FVariableExpression(null, bits), new FLiteralExpression(null, new FIntNLiteral(23, int32)));
            FFunctionCall expShift = FFunctionCall.create(null, uShiftR, arguments);
            arguments = asList(expShift, new FLiteralExpression(null, new FIntNLiteral(0xFF, int32)));
            FExpression exponent = FFunctionCall.create(null, aAnd, arguments);

            //mantissa: bits & 0x7FFFFF
            arguments = asList(new FVariableExpression(null, bits), new FLiteralExpression(null, new FIntNLiteral(0x7FFFFF, int32)));
            FExpression mantissa = FFunctionCall.create(null, aAnd, arguments);

            FReturn _return = FReturn.createTrusted(null, new Pack(null, asList(sign, exponent, mantissa)), splitRepresentation);
            splitRepresentation.setBody(FBlock.from(null, bitsDecl, _return));
        }
    }
}

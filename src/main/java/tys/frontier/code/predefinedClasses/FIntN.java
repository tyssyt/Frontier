package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.StaticField;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FIntIdentifier;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

import java.math.BigInteger;
import java.util.concurrent.ConcurrentMap;

public class FIntN extends FPredefinedClass {


    public static final FIdentifier SHIFT_L = new FIdentifier("shiftL");
    public static final FIdentifier U_SHIFT_R = new FIdentifier("uShiftR");
    public static final FIdentifier S_SHIFT_R = new FIdentifier("sShiftR");
    public static final FIdentifier S_MUL_OVERFLOW = new FIdentifier("signedMultiplicationWithOverflow");
    public static final FIdentifier U_MUL_OVERFLOW = new FIdentifier("unsignedMultiplicationWithOverflow");
    public static final FIdentifier COUNT_LEADING_ZEROS = new FIdentifier("countLeadingZeros");
    public static final FIdentifier COUNT_TRAILING_ZEROS = new FIdentifier("countTrailingZeros");

    //classes do not override equals, so we need to make sure we get the same object every time
    private static final ConcurrentMap<Integer, FIntN> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    public static final FIntN _8 = getIntN(8);
    public static final FIntN _16 = getIntN(16);
    public static final FIntN _32 = getIntN(32);
    public static final FIntN _64 = getIntN(64);

    static {FFloat32.INSTANCE.addIntFunctions(_32, _64);}
    static {FFloat64.INSTANCE.addIntFunctions(_32, _64);}

    private int n;
    private FFunction uShiftR;

    //TODO @PositionForGeneratedCode
    private FIntN(int n) {
        super(new FIntIdentifier(n));
        assert n>=4;
        this.n = n;
        addPredefinedFunctionsForArithType();
        addPredefinedFunctionsForIntType();

        DefaultNamespace namespace = this.getNamespace();
        StaticField max = new StaticField(null, MAX, this, namespace, FVisibilityModifier.EXPORT, true);
        max.setAssignmentTrusted(new FLiteralExpression(null, new FIntNLiteral(maxValue(), this)));
        namespace.addFieldTrusted(max);
        StaticField min = new StaticField(null, MIN, this, namespace, FVisibilityModifier.EXPORT, true);
        min.setAssignmentTrusted(new FLiteralExpression(null, new FIntNLiteral(minValue(), this)));
        namespace.addFieldTrusted(min);

        FunctionBuilder builder = new FunctionBuilder().setMemberOf(getNamespace()).setPredefined(true).setParams(this);
        namespace.addFunctionTrusted(builder.setIdentifier(TO_FLOAT32).setReturnType(FFloat32.INSTANCE).build());
        namespace.addFunctionTrusted(builder.setIdentifier(TO_FLOAT64).setReturnType(FFloat64.INSTANCE).build());
        if (n > 8)
            namespace.addFunctionTrusted(builder.setIdentifier(TO_CHAR).setReturnType(FIntN._8).build());
        if (n > 32)
            namespace.addFunctionTrusted(builder.setIdentifier(TO_INT32).setReturnType(FIntN._32).build());
        if (n > 64)
            namespace.addFunctionTrusted(builder.setIdentifier(TO_INT64).setReturnType(FIntN._64).build());
    }

    private void addPredefinedFunctionsForIntType() {
        DefaultNamespace namespace = this.getNamespace();

        try {
            BinaryOperator.AAND.addPredefined(this, this);
            BinaryOperator.AOR.addPredefined(this, this);
            BinaryOperator.XOR.addPredefined(this, this);
        } catch (SignatureCollision e) {
            Utils.cantHappen();
        }

        FunctionBuilder builder = new FunctionBuilder(null, namespace).setPredefined(true).setReturnType(this);

        builder.setParams(this);
        namespace.addFunctionTrusted(builder.setIdentifier(COUNT_LEADING_ZEROS).build());
        namespace.addFunctionTrusted(builder.setIdentifier(COUNT_TRAILING_ZEROS).build());

        builder.setParams(this, this);
        namespace.addFunctionTrusted(builder.setIdentifier(SHIFT_L).build());
        uShiftR = builder.setIdentifier(U_SHIFT_R).build();
        namespace.addFunctionTrusted(uShiftR);
        namespace.addFunctionTrusted(builder.setIdentifier(S_SHIFT_R).build());

        builder.setReturnType(FTuple.from(this, FBool.INSTANCE));
        namespace.addFunctionTrusted(builder.setIdentifier(S_MUL_OVERFLOW).build());
        namespace.addFunctionTrusted(builder.setIdentifier(U_MUL_OVERFLOW).build());
    }

    public static FIntN getIntN(int n) {
        return existing.computeIfAbsent(n, FIntN::new);
    }

    public int getN() {
        return n;
    }

    public FFunction getUShiftR() {
        return uShiftR;
    }

    @Override
    public boolean canImplicitlyCast() {
        return true;
    }

    public BigInteger minValue() {
        return maxValue().negate();
    }

    public BigInteger maxValue() {
        return BigInteger.valueOf(2).pow(n-1).subtract(BigInteger.ONE);
    }

    public static int neededBits(BigInteger number) {
        return Math.max(number.abs().bitLength()+1, 4); //no int representation smaller then 4 bits allowed, also need to add the "sign bit"
    }

    public boolean canRepresent(BigInteger i) {
        return maxValue().compareTo(i) >= 0 && i.compareTo(minValue()) >= 0;
    }
}

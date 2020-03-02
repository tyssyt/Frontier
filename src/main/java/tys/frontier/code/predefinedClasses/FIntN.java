package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FIntIdentifier;
import tys.frontier.code.literal.FIntNLiteral;

import java.math.BigInteger;
import java.util.concurrent.ConcurrentMap;

public class FIntN extends FPredefinedClass {

    public static final FIdentifier MAX = new FIdentifier("max");
    public static final FIdentifier MIN = new FIdentifier("min");

    //classes do not override equals, so we need to make sure we get the same object every time
    private static final ConcurrentMap<Integer, FIntN> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    public static final FIntN _8 = getIntN(8);
    public static final FIntN _16 = getIntN(16);
    public static final FIntN _32 = getIntN(32);
    public static final FIntN _64 = getIntN(64);

    private int n;

    private FIntN(int n) {
        super(new FIntIdentifier(n));
        assert n>=4;
        this.n = n;
        addPredefinedFunctionsForArithType();
        addPredefinedFunctionsForIntType();
        FField max = new FField(MAX, this, this, FVisibilityModifier.EXPORT, true, true);
        max.setAssignmentTrusted(new FLiteralExpression(new FIntNLiteral(maxValue(), this, "" + maxValue())));
        addFieldTrusted(max);
        FField min = new FField(MIN, this, this, FVisibilityModifier.EXPORT, true, true);
        min.setAssignmentTrusted(new FLiteralExpression(new FIntNLiteral(minValue(), this, "" + minValue())));
        addFieldTrusted(min);
    }

    public static FIntN getIntN(int n) {
        return existing.computeIfAbsent(n, FIntN::new);
    }

    public int getN() {
        return n;
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

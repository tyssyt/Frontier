package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIntIdentifier;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

import java.math.BigInteger;
import java.util.concurrent.ConcurrentMap;

public class FIntN extends FPredefinedClass {

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

        try {
            addFunction(UnaryOperator.NEG.createPredefined(this, this));

            addFunction(BinaryOperator.EQUALS.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.NOT_EQUALS.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.EQUALS_ID.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.NOT_EQUALS_ID.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.LESS.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.LESS_EQUAL.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.GREATER.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.GREATER_EQUAL.createPredefined(this, this, FBool.INSTANCE));

            addFunction(BinaryOperator.PLUS.createPredefined(this, this, this));
            addFunction(BinaryOperator.MINUS.createPredefined(this, this, this));
            addFunction(BinaryOperator.TIMES.createPredefined(this, this, this));
            addFunction(BinaryOperator.DIVIDED.createPredefined(this, this, this));
            addFunction(BinaryOperator.MODULO.createPredefined(this, this, this));
        } catch (SignatureCollision signatureCollision) {
            Utils.handleException(signatureCollision);
        }
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

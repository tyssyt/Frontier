package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.function.operator.FBinaryOperator;
import tys.frontier.code.function.operator.FUnaryOperator;
import tys.frontier.code.identifier.FIntIdentifier;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

import java.math.BigInteger;
import java.util.concurrent.ConcurrentMap;

public class FIntN extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static final ConcurrentMap<Integer, FIntN> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    public static final FIntN _8 = getIntN(8);
    public static final FIntN _32 = getIntN(32);
    public static final FIntN _64 = getIntN(64);

    private int n;

    private FIntN(int n) {
        super(new FIntIdentifier(n));
        assert n>=4;
        this.n = n;

        try {
            addFunction(FUnaryOperator.Pre.NEG.createPredefined(this));
            addFunction(FUnaryOperator.Pre.INC.createPredefined(this));
            addFunction(FUnaryOperator.Pre.DEC.createPredefined(this));

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

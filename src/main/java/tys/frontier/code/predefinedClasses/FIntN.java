package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.Operator.FUnaryOperator;
import tys.frontier.code.identifier.FIntIdentifier;
import tys.frontier.parser.syntaxErrors.SignatureCollision;

import java.util.concurrent.ConcurrentMap;

public class FIntN extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static final ConcurrentMap<Integer, FIntN> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    public static final FIntN _8 = getIntN(8);
    public static final FIntN _32 = getIntN(32);
    public static final FIntN _64 = getIntN(64);

    private int n;

    public int getN() {
        return n;
    }

    private FIntN(int n) {
        super(new FIntIdentifier(n));
        this.n = n;

        try {
            addFunction(FUnaryOperator.Pre.NEG.createPredefined(this));
            addFunction(FUnaryOperator.Pre.INC.createPredefined(this));
            addFunction(FUnaryOperator.Pre.DEC.createPredefined(this));
            addFunction(FUnaryOperator.Post.INC.createPredefined(this));
            addFunction(FUnaryOperator.Post.DEC.createPredefined(this));

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
            throw new RuntimeException(signatureCollision);
        }
    }

    public static FIntN getIntN(int n) {
        return existing.computeIfAbsent(n, FIntN::new);
    }
}

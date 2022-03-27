package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FFunctionTypeIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.util.Pair;

import java.util.concurrent.ConcurrentMap;

public class FFunctionType extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<Pair<FType, FType>, FFunctionType> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType in;
    private FType out;

    private FFunctionType(FType in, FType out) {
        super(new FFunctionTypeIdentifier(in, out));
        addDefaultFunctions();
        this.in = in;
        this.out = out;
    }

    @Override
    public boolean canImplicitlyCast() { //TODO remove once function types are implemented with parameterized classes
        return in != FTuple.VOID || out.canImplicitlyCast();
    }

    public FType getIn() {
        return in;
    }

    public FType getOut() {
        return out;
    }

    public static FFunctionType from(FType in, FType out) {
        return existing.computeIfAbsent(new Pair<>(in, out), p -> new FFunctionType(in, out));
    }

    public static FFunctionType from(Signature signature) {
        FType in = FTuple.fromExpressionList(signature.getParameters());
        return existing.computeIfAbsent(new Pair<>(in, signature.getType()), p -> new FFunctionType(p.a, p.b));
    }
}

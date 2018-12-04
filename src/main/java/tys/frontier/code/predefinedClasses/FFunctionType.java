package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.FFunction;
import tys.frontier.code.FType;
import tys.frontier.code.identifier.FFunctionTypeIdentifier;
import tys.frontier.util.Pair;

import java.util.List;
import java.util.concurrent.ConcurrentMap;

public class FFunctionType extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<Pair<List<FType>, FType>, FFunctionType> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private List<FType> in;
    private FType out;

    private FFunctionType(List<FType> in, FType out) {
        super(new FFunctionTypeIdentifier(in, out));
        this.in = in;
        this.out = out;
    }

    public List<FType> getIn() {
        return in;
    }

    public FType getOut() {
        return out;
    }

    public static FFunctionType from(List<FType> in, FType out) {
        return existing.computeIfAbsent(new Pair<>(in, out), p -> new FFunctionType(in, out));
    }

    public static FFunctionType from(FFunction function) {
        return existing.computeIfAbsent(new Pair<>(function.getSignature().getAllParamTypes(), function.getType()), p -> new FFunctionType(p.a, p.b));
    }
}

package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FFunctionTypeIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.util.Pair;
import tys.frontier.util.Triple;
import tys.frontier.util.Utils;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;

public class FFunctionType extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<Pair<FType, FType>, FFunctionType> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType in;
    private FType out;

    private FFunctionType(FType in, FType out) {
        super(new FFunctionTypeIdentifier(in, out));
        this.in = in;
        this.out = out;
    }

    @Override
    public long concreteness() { //TODO remove once function types are implemented with parameterized classes
        long res = Long.min(in.concreteness(), out.concreteness());
        if (res == Long.MAX_VALUE) //avoid overflow
            return Long.MAX_VALUE;
        return res+1;
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

    public static Triple<List<FType>, FType, TypeInstantiation> instantiableFrom(Signature signature) { //TODO this was butchered because I needed the List of parameters non-flattened
        if (signature.getFunction().getParameters().isEmpty())
            return new Triple<>(Utils.typesFromExpressionList(signature.getParameters()), signature.getType(), TypeInstantiation.EMPTY);

        //create a type instantiation mapping params to non fixed copies
        boolean baseFinished = signature.getFunction().getBody().isPresent();
        Map<FTypeVariable, FTypeVariable> varMap = new LinkedHashMap<>(); //linked map so newParametersList retains base order
        for (FTypeVariable var : signature.getFunction().getParametersList()) {
            if (!var.isFixed() && !baseFinished)
                Utils.NYI("getting a function address with non fixed Parameters where the body is not finished"); //TODO for non recursive cases, this could be solved by waiting on f to finish parsing
            FTypeVariable copy = var.copy(false);
            varMap.put(var, copy);
        }
        @SuppressWarnings({"rawtypes", "unchecked"}) TypeInstantiation typeInstantiation = TypeInstantiation.create((Map)varMap);

        //no need to store, the entire point of this function is to return a fresh version of the types on each call
        List<FType> in = Utils.typesFromExpressionList(signature.getParameters(), typeInstantiation::getType);
        return new Triple<>(in, typeInstantiation.getType(signature.getType()), typeInstantiation);
    }
}

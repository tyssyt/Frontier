package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionTypeIdentifier;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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

    @Override
    public long concreteness() { //TODO remove once function types are implemented with parameterized classes
        long res = out.concreteness();
        for (FType type : in) {
            res = Long.min(res, type.concreteness());
        }
        if (res == Long.MAX_VALUE) //avoid overflow
            return Long.MAX_VALUE;
        return res+1;
    }

    @Override
    public boolean canImplicitlyCast() { //TODO remove once function types are implemented with parameterized classes
        if (!in.isEmpty())
            return true;
        return out.canImplicitlyCast();
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

    public static Pair<FFunctionType, TypeInstantiation> instantiableFrom(FFunction function) {
        if (function.getParameters().isEmpty())
            return new Pair<>(from(function), TypeInstantiation.EMPTY);

        //create a type instantiation mapping params to non fixed copies
        boolean baseFinished = function.getBody().isPresent();
        Map<FTypeVariable, FTypeVariable> varMap = new LinkedHashMap<>(); //linked map so newParametersList retains base order
        for (FTypeVariable var : function.getParametersList()) {
            if (!var.isFixed() && !baseFinished)
                Utils.NYI("getting a function address with non fixed Parameters where the body is not finished"); //TODO for non recursive cases, this could be solved by waiting on f to finish parsing
            FTypeVariable copy = var.copy(false);
            varMap.put(var, copy);
        }
        //noinspection unchecked
        TypeInstantiation typeInstantiation = TypeInstantiation.create((Map)varMap);

        //no need to store, the entire point of this function is to return a fresh version of the types on each call
        return new Pair<>(
            new FFunctionType(Utils.typesFromExpressionList(function.getParams(), typeInstantiation::getType), typeInstantiation.getType(function.getType())),
            typeInstantiation
        );
    }
}

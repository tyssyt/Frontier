package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FFunctionTypeIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
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
    public int concreteness() { //TODO remove once function types are implemented with parameterized classes
        int res = Integer.min(in.concreteness(), out.concreteness());
        if (res == Integer.MAX_VALUE) //avoid overflow
            return Integer.MAX_VALUE;
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

    public static TypeInstantiation copyVarsForInstantiation(FFunction function) {
        if (function.getParameters().isEmpty())
            return TypeInstantiation.EMPTY;

        Map<FTypeVariable, FType> varMap = new HashMap<>();
        Map<TypeConstraints, TypeConstraints> constraintMap = new HashMap<>();
        Queue<TypeConstraints> todo = new ArrayDeque<>();
        boolean baseFinished = function.getBody().isPresent();
        for (FTypeVariable variable : function.getParametersList()) {
            if (!variable.isFixed() && !baseFinished)
                Utils.NYI("getting a function address with non fixed Parameters where the body is not finished"); //TODO for non recursive cases, this could be solved by waiting on f to finish parsing
            FTypeVariable copy = variable.shallowNonFixedCopy();
            varMap.put(variable, copy);
            todo.add(variable.getConstraints());
            constraintMap.put(variable.getConstraints(), copy.getConstraints());
        }

        while (!todo.isEmpty())
            todo.remove().copyContents(constraintMap, todo);

        return TypeInstantiation.create(varMap);
    }
}

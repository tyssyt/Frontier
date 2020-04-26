package tys.frontier.code.predefinedClasses;

import com.google.common.collect.Iterables;
import com.google.common.collect.MapMaker;
import tys.frontier.code.Typed;
import tys.frontier.code.identifier.FTupleIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentMap;

import static java.util.Arrays.asList;

public class FTuple extends FPredefinedClass {


    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<List<FType>, FTuple> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    public static final FTuple VOID = existing.computeIfAbsent(Collections.emptyList(), FTuple::new);

    private List<FType> types;

    private FTuple(List<FType> types) {
        super(new FTupleIdentifier(types));
        this.types = types;
    }

    @Override
    public long concreteness() {
        long res = 0;
        for (FType type : types) {
            res = Long.min(res, type.concreteness());
        }
        if (res == Long.MAX_VALUE) //avoid overflow
            return Long.MAX_VALUE;
        return res+1;
    }

    @Override
    public boolean canImplicitlyCast() {
        for (FType type : types) {
            if (type.canImplicitlyCast())
                return true;
        }
        return false;
    }

    public int arity() {
        return types.size();
    }

    public List<FType> getTypes() {
        return types;
    }

    public static FType from(FType... types) {
        return from(asList(types));
    }

    public static FType from(List<FType> types) {
        List<FType> flattened = new ArrayList<>();
        for (FType type : types) {
            if (type == VOID)
                return Utils.NYI("void in from flatten"); //maybe this can happen if I instantiate a type variable with void? Not sure what to do in that case...
            if (type instanceof FTuple)
                flattened.addAll(((FTuple) type).types);
            else
                flattened.add(type);
        }

        switch (flattened.size()) {
            case 0:
                return VOID;
            case 1:
                return Iterables.getOnlyElement(flattened);
            default:
                return existing.computeIfAbsent(flattened, FTuple::new);
        }
    }

    public static FType fromExpressionList (List<? extends Typed> exps) {
        return from(Utils.typesFromExpressionList(exps));
    }

    // Tuple related Utility Methods

    public static List<FType> unpackType(FType type) {
        return type instanceof FTuple ? ((FTuple) type).getTypes() : Collections.singletonList(type);
    }

    public static int arity(FType type) {
        return type instanceof FTuple ? ((FTuple) type).arity() : 1;
    }
}

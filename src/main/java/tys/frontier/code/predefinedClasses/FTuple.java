package tys.frontier.code.predefinedClasses;

import com.google.common.collect.Iterables;
import com.google.common.collect.MapMaker;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.InstanceField;
import tys.frontier.code.Typed;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTupleIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.loop.forImpl.TupleFor;
import tys.frontier.code.type.FType;
import tys.frontier.util.NameGenerator;
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

        DefaultNamespace namespace = getNamespace();
        NameGenerator names = new NameGenerator("", "");

        //add fields and getters
        //TODO @PositionForGeneratedCode
        for (FType fieldType : types) {
            InstanceField field = InstanceField.createTrusted(null, new FIdentifier(names.next()), fieldType, this, FVisibilityModifier.EXPORT, false, false);
            addFieldTrusted(field); //TODO make final
        }
        //remove setter TODO no longer needed when field final
        namespace.getFunctions(true).clear();

        //set for Impl
        setForImpl(new TupleFor());

        //TODO for homogenous tuples, I can add array access operators and for by idx
    }

    @Override
    public int concreteness() {
        int res = Integer.MAX_VALUE;
        for (FType type : types) {
            res = Integer.min(res, type.concreteness());
        }
        if (res == Integer.MAX_VALUE) //avoid overflow
            return Integer.MAX_VALUE;
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
                continue;
            if (type instanceof FTuple)
                flattened.addAll(((FTuple) type).types);
            else
                flattened.add(type);
        }

        return switch (flattened.size()) {
            case 0  -> VOID;
            case 1  -> Iterables.getOnlyElement(flattened);
            default -> existing.computeIfAbsent(flattened, FTuple::new);
        };
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

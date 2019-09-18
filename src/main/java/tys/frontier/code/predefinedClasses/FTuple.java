package tys.frontier.code.predefinedClasses;

import com.google.common.collect.Iterables;
import com.google.common.collect.MapMaker;
import tys.frontier.code.Typed;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FTupleIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentMap;

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

    public List<FType> getTypes() {
        return types;
    }

    public static FType from (List<FType> types) {
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

    public static void checkTypes(List<FExpression> expressions, List<FType> targetTypes) throws IncompatibleTypes {
        int j=0;
        for (int i=0; i < expressions.size(); i++) {
            FExpression exp = expressions.get(i);

            if (exp.getType() instanceof FTuple) {
                //pack target types to match tuple expression
                int size = ((FTuple) exp.getType()).getTypes().size();
                if (j+size > targetTypes.size())
                    throw new IncompatibleTypes(from(targetTypes), fromExpressionList(expressions));
                FType targetType = from(targetTypes.subList(j, j + size));
                expressions.set(i, exp.typeCheck(targetType));
                j += size;

            } else {
                //no tuple, we can single match
                if (j+1 > targetTypes.size())
                    throw new IncompatibleTypes(from(targetTypes), fromExpressionList(expressions));
                expressions.set(i, exp.typeCheck(targetTypes.get(j)));
                j++;
            }
        }
        if (j < targetTypes.size())
            throw new IncompatibleTypes(from(targetTypes), fromExpressionList(expressions));
    }
}

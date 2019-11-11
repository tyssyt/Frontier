package tys.frontier.util.expressionListToTypeListMapping;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.MultimapBuilder;
import com.pivovarit.function.ThrowingBiConsumer;
import tys.frontier.code.FParameter;
import tys.frontier.code.Typed;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.ArrayUtils;
import tys.frontier.util.Pair;
import tys.frontier.util.TransformedListIterator;
import tys.frontier.util.Utils;

import java.util.*;
import java.util.function.Function;

public class ArgMapping {

    private List<ImplicitTypeCast> casts;
    private BitSet unpackArg;
    private int[] packParam;
    private int numberOfParamsFilledWithPositionalArgs;

    //TODO make sure all functions in here have reasonable quickpaths for "non tuple" cases

    public ArgMapping(BitSet unpackArg, int[] packParam, int numberOfParamsFilledWithPositionalArgs) {
        this.unpackArg = unpackArg;
        this.packParam = packParam;
        this.numberOfParamsFilledWithPositionalArgs = numberOfParamsFilledWithPositionalArgs;
    }

    public static ArgMapping createCasted(List<FType> expressions, List<FType> target) throws TooManyArguments, UnfulfillableConstraints, IncompatibleTypes {
        BitSet unpackArg = new BitSet(expressions.size());
        int[] packParam = ArrayUtils.create(target.size(), 1);

        ListIterator<FType> argIt = expressions.listIterator();
        ListIterator<FType> paramIt = target.listIterator();

        unpackAndPack(true, unpackArg, packParam, argIt, paramIt);

        ArgMapping res = new ArgMapping(unpackArg, packParam, target.size());
        ListMultimap<FTypeVariable, TypeConstraint> constraints = res.computeCasts(expressions, target);
        TypeConstraint.addAll(constraints);
        return res;
    }
    public static Pair<ArgMapping, List<FType>> createForCall(List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, List<FParameter> params) throws NoArgumentsForParameter, TooManyArguments {
        BitSet unpackArg = new BitSet();
        int[] packParam = ArrayUtils.create(params.size(), 1);

        ListIterator<FType> argIt = positionalArgs.listIterator();
        ListIterator<FParameter> paramIt = params.listIterator();

        //start with positional args
        unpackAndPack(false, unpackArg, packParam, argIt, new TransformedListIterator<>(paramIt, Typed::getType));
        List<FType> argumentTypes = new ArrayList<>(positionalArgs);
        int numberOfParamsFilledWithPositionalArgs = paramIt.nextIndex();

        //fill the remaining params with keyword args or default values
        int usedKeywordArgs = 0;
        while (paramIt.hasNext()) {
            FParameter param = paramIt.next();

            List<FType> arg = keywordArgs.get(param.getIdentifier());
            if (!arg.isEmpty()) {
                if (arg.size() > 1)
                    packParam[paramIt.previousIndex()] = arg.size();
                argumentTypes.addAll(arg);
                usedKeywordArgs++;
                //TODO type check for the packed ones? or at least arity check?
            } else if (param.hasDefaultValue()) {
                argumentTypes.add(param.getType());
            } else {
                throw new NoArgumentsForParameter(param);
            }
        }

        if (usedKeywordArgs != keywordArgs.keySet().size())
            throw new TooManyArguments();

        return new Pair<>(new ArgMapping(unpackArg, packParam, numberOfParamsFilledWithPositionalArgs), argumentTypes);
    }

    // argTypes = paramTypes
    public static ArgMapping createBasic(List<FType> types, int numberOfParamsFilledWithPositionalArgs) {
        ArgMapping res = new ArgMapping(new BitSet(types.size()), ArrayUtils.create(types.size(), 1), numberOfParamsFilledWithPositionalArgs);
        res.casts = Arrays.asList(new ImplicitTypeCast[types.size()]);
        return res;
    }

    // no packaing/unpacking, types are casted
    public static ArgMapping createBasic(List<FType> positionalArgs, List<FType> target) throws IncompatibleTypes, UnfulfillableConstraints {
        assert positionalArgs.size() == target.size();
        ArgMapping res = new ArgMapping(new BitSet(positionalArgs.size()), ArrayUtils.create(positionalArgs.size(), 1), positionalArgs.size());
        ListMultimap<FTypeVariable, TypeConstraint> constraints = res.computeCasts(positionalArgs, target);
        TypeConstraint.addAll(constraints);
        return res;
    }

    private static void unpackAndPack(boolean useAllParams, BitSet unpackArg, int[] packParam, ListIterator<FType> argIt, ListIterator<FType> paramIt) throws TooManyArguments {
        while (argIt.hasNext() && paramIt.hasNext()) {
            FType arg = argIt.next();
            FType param = paramIt.next();
            int argArity = FTuple.arity(arg);
            int paramArity = FTuple.arity(param);
            if (argArity > paramArity) {
                paramIt.previous();
                if (unpack((FTuple) arg, paramIt) > 1)
                    unpackArg.set(argIt.previousIndex());
            } else if (argArity < paramArity) {
                argIt.previous();
                int packedArgs = pack(argIt, (FTuple) param, unpackArg);
                if (packedArgs > 1)
                    packParam[paramIt.previousIndex()] = packedArgs;
            } // else param is passed over without any packing/unpacking
        }

        if (argIt.hasNext())
            throw new TooManyArguments();
        if (useAllParams && paramIt.hasNext())
            Utils.NYI("???"); //TODO
    }

    private static int unpack(FTuple tuple, ListIterator<FType> params) throws TooManyArguments {
        int size = 0;
        int filledTypes = 0;

        while (size < tuple.arity()) {
            if (!params.hasNext())
                throw new TooManyArguments();

            size += FTuple.arity(params.next());
            filledTypes++;
        }

        if (size > tuple.arity())
            return Utils.handleError("unpacking does not fill param completly"); //TODO
        return filledTypes;
    }

    private static int pack(ListIterator<FType> expressions, FTuple tuple, BitSet unpackArg) {
        int size = 0;
        while (size < tuple.arity()) {
            if (!expressions.hasNext())
                return Utils.handleError("not enough positional arguments to fill tuple parameter"); //TODO

            FType next = expressions.next();
            if (size == 0 && next == FNull.NULL_TYPE) {
                /* TODO there are cases where the tuple should be packed but the first argument is null, we need a more complex 2 pass counting approach to distinguish these
                        some of the above cases would be covered by trying harder to type null, because if we have a typed null we can cover all cases that come from baking
                        the baking case is the reason we needed to introduce this in the first place, if we cover that we can switch the default behaviour back to first arg of tuple

                        However, once we start having data types we can have the same problem without baking were we actually have an untyped null
                        then ArgMapping actually is responsible for typing it.
                        If we introduce counting for the param side to handle generic functions we can also introduce it on arg side.

                        Once we extend the syntax to have brackets, and if counting cannot resolve ambiguity, assuming tuple null is better because the user can use brackets to specify null as first argument
                 */
                return 1; //null fills the entire tuple
            }

            if (next instanceof FTuple) {
                unpackArg.set(expressions.previousIndex()); //we need to first unpack tuples if they are packed
            }

            size += FTuple.arity(next);
        }
        return size;
    }

    public List<ImplicitTypeCast> getCasts() {
        return casts;
    }

    public boolean hasUnpacking() {
        return !unpackArg.isEmpty();
    }

    public boolean getUnpackArg(int i) {
        return unpackArg.get(i);
    }

    public boolean hasPacking() {
        for (int i : packParam) {
            if (i > 1)
                return true;
        }
        return false;
    }

    public boolean getPackParam(int i) {
        return packParam[i] > 1;
    }

    public int getNumberOfParamsFilledWithPositionalArgs() {
        return numberOfParamsFilledWithPositionalArgs;
    }

    public int getNUmberOfCasts() {
        return Utils.countNonNull(casts);
    }

    public int getCostsOfCasts() {
        int cost = 0;
        for (ImplicitTypeCast cast : casts) {
            if (cast != null)
                cost += cast.getCost();
        }
        return cost;
    }

    public ListMultimap<FTypeVariable, TypeConstraint> computeCasts(List<FType> argumentTypes, List<FType> target) throws IncompatibleTypes {
        casts = new ArrayList<>();
        ListMultimap<FTypeVariable, TypeConstraint> constraints = MultimapBuilder.hashKeys().arrayListValues().build();
        consumeUnpacked(argumentTypes, target, (baseType, targetType) -> {
            if (baseType != targetType)
                casts.add(ImplicitTypeCast.create(baseType, targetType, Variance.Covariant, constraints));
            else
                casts.add(null);
        });
        return constraints;
    }

    public <E extends Exception> void consumeUnpacked(List<FType> argumentTypes, List<FType> target, ThrowingBiConsumer<FType, FType, E> consumer) throws E {
        List<FType> unpackedBase = unpackBase(argumentTypes, FTuple::unpackType);
        List<FType> unpackedTarget = unpackTarget(target, FTuple::unpackType);
        assert unpackedBase.size() == unpackedTarget.size();
        for (Pair<FType, FType> pair : Utils.zip(unpackedBase, unpackedTarget))
            consumer.accept(pair.a, pair.b);
    }

    public <T> List<T> unpackBase(List<T> items, Function<T, List<T>> unpacker) {
        if (!hasUnpacking())
            return items;

        List<T> unpacked = new ArrayList<>();
        for (int i = 0; i < items.size(); i++) {
            T item = items.get(i);
            if (getUnpackArg(i)) {
                unpacked.addAll(unpacker.apply(item));
            } else {
                unpacked.add(item);
            }
        }
        return unpacked;
    }

    public <T> List<T> unpackTarget(List<T> items, Function<T, List<T>> unpacker) {
        if (!hasPacking())
            return items;

        List<T> unpacked = new ArrayList<>();
        for (int i = 0; i < items.size(); i++) {
            T item = items.get(i);
            if (getPackParam(i)) {
                unpacked.addAll(unpacker.apply(item));
            } else {
                unpacked.add(item);
            }
        }
        return unpacked;
    }

    public <T> List<T> pack(List<T> items, Function<List<T>, T> packer) {
        if (!hasPacking())
            return items;

        List<T> packed = new ArrayList<>(packParam.length);
        int p = 0;
        for (int i = 0; i < items.size(); p++) {
            int itemsToPack = packParam[p];
            if (itemsToPack > 1) {
                packed.add(packer.apply(items.subList(i, i + itemsToPack)));
                i += itemsToPack;
            } else {
                packed.add(items.get(i++));
            }
        }
        return packed;
    }

    public static class TooManyArguments extends SyntaxError {
        //TODO
    }

    public static class NoArgumentsForParameter extends SyntaxError {
        public final FParameter parameter;

        public NoArgumentsForParameter(FParameter parameter) {
            this.parameter = parameter;
        }
    }
}

package tys.frontier.util.expressionListToTypeListMapping;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.MultimapBuilder;
import tys.frontier.code.FParameter;
import tys.frontier.code.Typed;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.expression.cast.TypeParameterCast;
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
import tys.frontier.util.Pair;
import tys.frontier.util.TransformedListIterator;
import tys.frontier.util.Utils;

import java.util.*;

public class ArgMapping {

    private List<ImplicitTypeCast> casts;
    private BitSet unpackArg;
    private BitSet packParam;
    private int numberOfParamsFilledWithPositionalArgs;

    //TODO make sure all functions in here have reasonable quickpaths for "non tuple" cases

    public ArgMapping(BitSet unpackArg, BitSet packParam, int numberOfParamsFilledWithPositionalArgs) {
        this.unpackArg = unpackArg;
        this.packParam = packParam;
        this.numberOfParamsFilledWithPositionalArgs = numberOfParamsFilledWithPositionalArgs;
    }

    public static ArgMapping createCasted(List<FType> expressions, List<FType> target) throws TooManyArguments, UnfulfillableConstraints, IncompatibleTypes {
        BitSet unpackArg = new BitSet(expressions.size());
        BitSet packParam = new BitSet(target.size());

        ListIterator<FType> argIt = expressions.listIterator();
        ListIterator<FType> paramIt = target.listIterator();

        unpackAndPack(unpackArg, packParam, argIt, paramIt);

        ArgMapping res = new ArgMapping(unpackArg, packParam, target.size());
        ListMultimap<FTypeVariable, TypeConstraint> constraints = res.computeCasts(expressions, target);
        TypeConstraint.addAll(constraints);
        return res;
    }
    public static Pair<ArgMapping, List<FType>> createForCall(List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, List<FParameter> params) throws NoArgumentsForParameter, TooManyArguments {
        BitSet unpackArg = new BitSet();
        BitSet packParam = new BitSet(params.size());

        ListIterator<FType> argIt = positionalArgs.listIterator();
        ListIterator<FParameter> paramIt = params.listIterator();

        //start with positional args
        unpackAndPack(unpackArg, packParam, argIt, new TransformedListIterator<>(paramIt, Typed::getType));
        List<FType> argumentTypes = new ArrayList<>(positionalArgs);
        int numberOfParamsFilledWithPositionalArgs = paramIt.nextIndex();

        //fill the remaining params with keyword args or default values
        int usedKeywordArgs = 0;
        while (paramIt.hasNext()) {
            FParameter param = paramIt.next();

            List<FType> arg = keywordArgs.get(param.getIdentifier());
            if (!arg.isEmpty()) {
                if (arg.size() > 1)
                    packParam.set(paramIt.previousIndex());
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
        ArgMapping res = new ArgMapping(new BitSet(types.size()), new BitSet(types.size()), numberOfParamsFilledWithPositionalArgs);
        res.casts = Arrays.asList(new ImplicitTypeCast[types.size()]);
        return res;
    }

    // no packaing/unpacking, types are casted
    public static ArgMapping createBasic(List<FType> positionalArgs, List<FType> target) throws IncompatibleTypes, UnfulfillableConstraints {
        assert positionalArgs.size() == target.size();
        ArgMapping res = new ArgMapping(new BitSet(positionalArgs.size()), new BitSet(positionalArgs.size()), positionalArgs.size());
        ListMultimap<FTypeVariable, TypeConstraint> constraints = res.computeCasts(positionalArgs, target);
        TypeConstraint.addAll(constraints);
        return res;
    }

    private static void unpackAndPack(BitSet unpackArg, BitSet packParam, ListIterator<FType> argIt, ListIterator<FType> paramIt) throws TooManyArguments {
        while (argIt.hasNext()) {
            FType arg = argIt.next();
            if (arg instanceof FTuple) {
                if (unpack((FTuple) arg, paramIt) > 1)
                    unpackArg.set(argIt.previousIndex());
            } else {
                if (!paramIt.hasNext())
                    throw new TooManyArguments();

                FType param = paramIt.next();
                if (param instanceof FTuple) {
                    argIt.previous();
                    if (pack(argIt, (FTuple) param) > 1)
                        packParam.set(paramIt.previousIndex());
                }
            }
        }
    }

    private static int unpack(FTuple tuple, ListIterator<FType> params) throws TooManyArguments {
        int size = tuple.arity();
        int filledTypes = 0;

        while (size > 0) {
            if (!params.hasNext())
                throw new TooManyArguments();

            size -= FTuple.arity(params.next());
            filledTypes++;
        }

        if (size < 0)
            return Utils.handleError("unpacking does not fill param completly"); //TODO
        return filledTypes;
    }

    private static int pack(ListIterator<FType> expressions, FTuple tuple) {
        for (int size=0; size<tuple.arity(); size++) {
            if (!expressions.hasNext())
                return Utils.handleError("not enough positional arguments to fill tuple parameter"); //TODO

            FType next = expressions.next();
            if (next instanceof FTuple)
                return Utils.handleError("mixed filling for tuple parameter"); //TODO

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
        }
        return tuple.getTypes().size();
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
        return !packParam.isEmpty();
    }

    public boolean getPackParam(int i) {
        return packParam.get(i);
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
        //TODO I could optimize this by storing more info when computing packing
        ListMultimap<FTypeVariable, TypeConstraint> constraints = MultimapBuilder.hashKeys().arrayListValues().build();
        casts = new ArrayList<>();

        int targetIndex = 0;
        int argIndex = 0;
        while (argIndex < argumentTypes.size()) {
            if (unpackArg.get(argIndex)) {
                for (FType baseType : FTuple.unpackType(argumentTypes.get(argIndex++))) {
                    //TODO I think in theory I could also repack the unpacked args? like split a 4 arity in to 2-1-1?
                    assert !packParam.get(targetIndex);
                    FType targetType = target.get(targetIndex++);
                    if (baseType != targetType)
                        casts.add(TypeParameterCast.create(baseType, targetType, Variance.Covariant, constraints));
                    else
                        casts.add(null);
                }
            } else if (packParam.get(targetIndex)) { //TODO see above note onto first unpack then repack
                for (FType targetType : FTuple.unpackType(target.get(targetIndex++))) {
                    assert !unpackArg.get(argIndex);
                    FType baseType = argumentTypes.get(argIndex++);
                    if (baseType != targetType)
                        casts.add(TypeParameterCast.create(baseType, targetType, Variance.Covariant, constraints));
                    else
                        casts.add(null);
                }
            } else {
                FType argType = argumentTypes.get(argIndex++);
                FType targetType = target.get(targetIndex++);
                if (argType != targetType)
                    casts.add(TypeParameterCast.create(argType, targetType, Variance.Covariant, constraints));
                else
                    casts.add(null);
            }
        }
        assert targetIndex == target.size();

        return constraints;
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

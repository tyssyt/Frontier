package tys.frontier.util.expressionListToTypeListMapping;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.MultimapBuilder;
import tys.frontier.code.FParameter;
import tys.frontier.code.Typed;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.expression.cast.TypeParameterCast;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Utils;

import java.util.*;

public class ArgMapping {

    private List<FType> argumentTypes; //TODO fairly certain I can remove this field
    private List<ImplicitTypeCast> casts;
    private BitSet unpackArg;
    private BitSet packParam;
    private int numberOfParamsFilledWithPositionalArgs;

    //TODO make sure all functions in here have reasonable quickpaths for "non tuple" cases

    public ArgMapping(List<FType> argumentTypes, BitSet unpackArg, BitSet packParam, int numberOfParamsFilledWithPositionalArgs) {
        this.argumentTypes = argumentTypes;
        this.unpackArg = unpackArg;
        this.packParam = packParam;
        this.numberOfParamsFilledWithPositionalArgs = numberOfParamsFilledWithPositionalArgs;
    }

    public static ArgMapping createCasted(List<FType> expressions, List<? extends Typed> target) throws TooManyArguments, UnfulfillableConstraints, IncompatibleTypes {
        BitSet unpackArg = new BitSet(expressions.size());
        BitSet packParam = new BitSet(target.size());

        ListIterator<FType> argIt = expressions.listIterator();
        ListIterator<? extends Typed> paramIt = target.listIterator();

        unpackAndPack(unpackArg, packParam, argIt, paramIt);

        ArgMapping res = new ArgMapping(expressions, unpackArg, packParam, target.size());
        ListMultimap<FTypeVariable, TypeConstraint> constraints = res.computeCasts(Utils.typesFromExpressionList(target));
        TypeConstraint.addAll(constraints);
        return res;
    }
    public static ArgMapping createForCall(List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, List<FParameter> params) throws NoArgumentsForParameter, TooManyArguments {
        BitSet unpackArg = new BitSet();
        BitSet packParam = new BitSet(params.size());

        ListIterator<FType> argIt = positionalArgs.listIterator();
        ListIterator<FParameter> paramIt = params.listIterator();

        //start with positional args
        unpackAndPack(unpackArg, packParam, argIt, paramIt);
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

        return new ArgMapping(argumentTypes, unpackArg, packParam, numberOfParamsFilledWithPositionalArgs);
    }

    // argTypes = paramTypes
    public static ArgMapping createBasic(List<FType> types, int numberOfParamsFilledWithPositionalArgs) {
        ArgMapping res = new ArgMapping(types, new BitSet(types.size()), new BitSet(types.size()), numberOfParamsFilledWithPositionalArgs);
        res.casts = Arrays.asList(new ImplicitTypeCast[types.size()]);
        return res;
    }

    // no packaing/unpacking, types are casted
    public static ArgMapping createBasic(List<FType> positionalArgs, List<FType> target) throws IncompatibleTypes, UnfulfillableConstraints {
        assert positionalArgs.size() == target.size();
        ArgMapping res = new ArgMapping(positionalArgs, new BitSet(positionalArgs.size()), new BitSet(positionalArgs.size()), positionalArgs.size());
        ListMultimap<FTypeVariable, TypeConstraint> constraints = res.computeCasts(target);
        TypeConstraint.addAll(constraints);
        return res;
    }

    private static void unpackAndPack(BitSet unpackArg, BitSet packParam, ListIterator<FType> argIt, ListIterator<? extends Typed> paramIt) throws TooManyArguments {
        while (argIt.hasNext()) {
            FType arg = argIt.next();
            if (arg instanceof FTuple) {
                if (unpack((FTuple) arg, paramIt) > 1)
                    unpackArg.set(argIt.previousIndex());
            } else {
                if (!paramIt.hasNext())
                    throw new TooManyArguments();

                FType param = paramIt.next().getType();
                if (param instanceof FTuple) {
                    argIt.previous();
                    pack(argIt, (FTuple) param);
                    packParam.set(paramIt.previousIndex());
                }
            }
        }
    }

    private static int unpack(FTuple tuple, Iterator<? extends Typed> params) throws TooManyArguments {
        int size = tuple.arity();
        int filledTypes = 0;

        while (size > 0) {
            if (!params.hasNext())
                throw new TooManyArguments();

            size -= FTuple.arity(params.next().getType());
            filledTypes++;
        }

        if (size < 0)
            return Utils.handleError("unpacking does not fill param completly"); //TODO
        return filledTypes;
    }

    private static int pack(ListIterator<FType> expressions, FTuple tuple) {
        for (int size=tuple.arity(); size>0; size--) {
            if (!expressions.hasNext())
                return Utils.handleError("not enough positional arguments to fill tuple parameter"); //TODO

            if (expressions.next() instanceof FTuple)
                return Utils.handleError("mixed filling for tuple parameter"); //TODO
        }
        return tuple.getTypes().size();
    }

    public List<FType> getArgumentTypes() {
        return argumentTypes;
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

    public ListMultimap<FTypeVariable, TypeConstraint> computeCasts(List<FType> target) throws IncompatibleTypes {
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

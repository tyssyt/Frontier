package tys.frontier.util.expressionListToTypeListMapping;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.MultimapBuilder;
import tys.frontier.code.FParameter;
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

public class ExpressionListToTypeListMapping {

    private List<FType> argumentTypes;
    private List<ImplicitTypeCast> casts;
    private boolean hasUnpacking;
    private BitSet unpackArg;
    private boolean hasPacking;
    private BitSet packParam;
    private int numberOfParamsFilledWithPositionalArgs;

    //TODO make sure all functions in here have reasonable quickpaths for "non tuple" cases

    public static ExpressionListToTypeListMapping create(List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, List<FParameter> params) throws NoArgumentsForParameter, TooManyArguments {
        boolean hasUnpacking = false;
        BitSet unpackArg = new BitSet();
        boolean hasPacking = false;
        BitSet packParam = new BitSet(params.size());

        //start with positional args
        ListIterator<FType> argIt = positionalArgs.listIterator();
        ListIterator<FParameter> paramIt = params.listIterator();
        while (argIt.hasNext()) {
            FType arg = argIt.next();
            if (arg instanceof FTuple) {
                unpack((FTuple)arg, paramIt);
                hasUnpacking = true;
                unpackArg.set(argIt.previousIndex());
            } else {
                if (!paramIt.hasNext())
                    throw new TooManyArguments();

                FType param = paramIt.next().getType();
                if (param instanceof FTuple) {
                    argIt.previous();
                    pack(argIt, (FTuple)param);
                    hasPacking = true;
                    packParam.set(paramIt.previousIndex());
                }
            }
        }
        List<FType> argumentTypes = new ArrayList<>(positionalArgs);
        int numberOfParamsFilledWithPositionalArgs = paramIt.nextIndex();

        //fill the remaining params with keyword args or default values
        int usedKeywordArgs = 0;
        while (paramIt.hasNext()) {
            FParameter param = paramIt.next();

            FType arg = keywordArgs.get(param.getIdentifier());
            if (arg != null) {
                argumentTypes.add(arg);
                usedKeywordArgs++;
            } else if (param.hasDefaultValue()) {
                argumentTypes.add(param.getType());
            } else {
                throw new NoArgumentsForParameter(param);
            }
        }

        if (usedKeywordArgs != keywordArgs.size())
            throw new TooManyArguments();

        ExpressionListToTypeListMapping res = new ExpressionListToTypeListMapping();
        res.argumentTypes = argumentTypes;
        res.hasUnpacking = hasUnpacking;
        res.unpackArg = unpackArg;
        res.hasPacking = hasPacking;
        res.packParam = packParam;
        res.numberOfParamsFilledWithPositionalArgs = numberOfParamsFilledWithPositionalArgs;
        return res;
    }

    // argTypes = paramTypes
    public static ExpressionListToTypeListMapping createBasic(List<FType> types, int numberOfParamsFilledWithPositionalArgs) {
        ExpressionListToTypeListMapping res = new ExpressionListToTypeListMapping();
        res.argumentTypes = types;
        res.unpackArg = new BitSet(types.size());
        res.packParam = new BitSet(types.size());
        res.casts = Arrays.asList(new ImplicitTypeCast[types.size()]);
        res.numberOfParamsFilledWithPositionalArgs = numberOfParamsFilledWithPositionalArgs;
        return res;
    }

    // no packaing/unpacking, types are casted
    public static ExpressionListToTypeListMapping createBasic(List<FType> positionalArgs, List<FType> target) throws IncompatibleTypes, UnfulfillableConstraints {
        assert positionalArgs.size() == target.size();
        ExpressionListToTypeListMapping res = new ExpressionListToTypeListMapping();
        res.argumentTypes = positionalArgs;
        res.unpackArg = new BitSet(positionalArgs.size());
        res.packParam = new BitSet(positionalArgs.size());
        res.numberOfParamsFilledWithPositionalArgs = positionalArgs.size();
        ListMultimap<FTypeVariable, TypeConstraint> constraints = res.computeCasts(target);
        TypeConstraint.addAll(constraints);
        return res;
    }

    private static int unpack(FTuple tuple, Iterator<FParameter> params) throws TooManyArguments {
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
        return hasUnpacking;
    }

    public boolean getUnpackArg(int i) {
        return unpackArg.get(i);
    }

    public boolean hasPacking() {
        return hasPacking;
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
        ListMultimap<FTypeVariable, TypeConstraint> constraints = MultimapBuilder.hashKeys().arrayListValues().build();

        casts = Arrays.asList(new ImplicitTypeCast[argumentTypes.size()]);
        int targetIndex = 0;
        for (int i = 0; i < argumentTypes.size(); i++) {
            FType argType = argumentTypes.get(i);
            if (argType instanceof FTuple) {
                FTuple baseType = (FTuple) argType;
                FTuple targetType = (FTuple) FTuple.from(target.subList(targetIndex, targetIndex + baseType.arity()));
                if (argType != targetType) {
                    casts.set(i, TypeParameterCast.createTPC(baseType, targetType, Variance.Covariant, constraints));
                }
                targetIndex += baseType.arity();
            } else {
                FType targetType = target.get(targetIndex);
                if (argType != targetType) {
                    casts.set(i, ImplicitTypeCast.create(argType, targetType, Variance.Covariant, constraints));
                }
                targetIndex++;
            }
        }

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

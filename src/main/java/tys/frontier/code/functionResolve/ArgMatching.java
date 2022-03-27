package tys.frontier.code.functionResolve;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FParameter;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.NotEnoughArguments;
import tys.frontier.parser.syntaxErrors.TooManyArguments;

import java.util.*;
import java.util.function.Function;

import static tys.frontier.code.predefinedClasses.FTuple.arity;
import static tys.frontier.util.Utils.typesFromExpressionList;

public class ArgMatching {

    private BitSet unpackArg;
    private List<? extends IdentifierNameable> parametersByName;

    private ArgMatching(BitSet unpackArg, List<? extends IdentifierNameable> parametersByName) {
        this.unpackArg = unpackArg;
        this.parametersByName = parametersByName;
    }

    public static ArgMatching dummy(int numberOfPositionalArgs, List<? extends IdentifierNameable> namedArgs) {
        return new ArgMatching(new BitSet(numberOfPositionalArgs), namedArgs);
    }

    public static ArgMatching create(List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, ImmutableList<FParameter> parameters) throws TooManyArguments, NotEnoughArguments, IncompatibleTypes {
        ListIterator<FParameter> paramIt = parameters.listIterator();

        //start with matching positional args, potentially unpacking them if necessary
        BitSet unpack = new BitSet(positionalArgs.size());
        int argIdx = 0;
        for (FType arg : positionalArgs) {
            if (!paramIt.hasNext())
                throw new TooManyArguments(arg);

            int argArity = FTuple.arity(arg);
            FType param = paramIt.next().getType();
            int paramArity = FTuple.arity(param);
            assert argArity == paramArity || paramArity == 1;

            //unpack if necessary
            if (argArity > paramArity) {
                assert arg instanceof FTuple;
                unpack.set(argIdx);
                //remove the next argArity-1 elements from parameters
                for (int i = 1; i < argArity; i++) {
                    if (!paramIt.hasNext() || arity(paramIt.next().getType()) > 1) // side effect: next() gets called within the if!
                        throw new IncompatibleTypes(param, arg);
                }
            }

            argIdx++;
        }
        int endOfPositional = paramIt.nextIndex();

        //fill the missing params with keyword args or default values
        int usedKeywordArgs = 0;
        while (paramIt.hasNext()) {
            FParameter param = paramIt.next();
            if (keywordArgs.get(param.getIdentifier()) != null)
                usedKeywordArgs++; //use keyword arg
            else if (!param.hasDefaultValue())
                throw new NotEnoughArguments("No Arguments for Parameter", param);
        }

        //check if all keyword arguments were used
        if (usedKeywordArgs != keywordArgs.keySet().size())
            throw new TooManyArguments(Iterables.concat(positionalArgs, keywordArgs.values()), typesFromExpressionList(parameters));

        return new ArgMatching(unpack, parameters.subList(endOfPositional, parameters.size()));
    }

    public <T> List<T> createArgList(List<T> positionalArgs, Map<FIdentifier, T> keywordArgs, Function<T, List<? extends T>> unpacker) {
        List<T> argList = new ArrayList<>();
        int i = 0;
        // start with positional args, unpacking if necessary
        for (T arg : positionalArgs) {
            if (unpackArg.get(i))
                argList.addAll(unpacker.apply(arg));
            else
                argList.add(arg);
        }
        // then keyword and default args
        for (IdentifierNameable param : parametersByName) {
            argList.add(keywordArgs.get(param.getIdentifier())); // adds null if default arg is used, which is exactly what we need
        }
        return argList;
    }

}

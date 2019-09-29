package tys.frontier.util.expressionListToTypeListMapping;

import tys.frontier.code.FParameter;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.util.Utils;

import java.util.*;

public class ExpressionListToTypeListMapping {

    private FType glue;

    public static ExpressionListToTypeListMapping create(List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, List<FParameter> params) throws NoArgumentsForParameter, TooManyArguments {
        //start with positional args
        ListIterator<FType> argIt = positionalArgs.listIterator();
        ListIterator<FParameter> paramIt = params.listIterator();
        while (argIt.hasNext()) {
            FType arg = argIt.next();
            if (arg instanceof FTuple) {
                unpack((FTuple)arg, paramIt);
            } else {
                if (!paramIt.hasNext())
                    throw new TooManyArguments();

                FType param = paramIt.next().getType();
                if (param instanceof FTuple) {
                    argIt.previous();
                    pack(argIt, (FTuple)param);
                } else {
                    //TODO one to one with maybe casting
                }

            }
        }
        List<FType> sthWeird = new ArrayList<>(positionalArgs);

        //fill the remaining params with keyword args or default values
        int usedKeywordArgs = 0;
        while (paramIt.hasNext()) {
            FParameter param = paramIt.next();

            FType arg = keywordArgs.get(param.getIdentifier());
            if (arg != null) {
                sthWeird.add(arg);
                usedKeywordArgs++;
            } else if (param.hasDefaultValue()) {
                sthWeird.add(param.getType());
            } else {
                throw new NoArgumentsForParameter(param);
            }
        }

        if (usedKeywordArgs != keywordArgs.size())
            throw new TooManyArguments();

        ExpressionListToTypeListMapping res = new ExpressionListToTypeListMapping();
        res.glue = FTuple.from(sthWeird);
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

    public FType getGlue() {
        return glue;
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

package tys.frontier.code;

import com.google.common.collect.*;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.IntIntPair;
import tys.frontier.util.Pair;
import tys.frontier.util.StringBuilderToString;
import tys.frontier.util.Utils;

import java.util.List;

public abstract class FType implements IdentifierNameable, StringBuilderToString {
    protected FTypeIdentifier identifier;
    protected BiMap<FIdentifier, FField> instanceFields = HashBiMap.create();
    protected BiMap<FIdentifier, FField> staticFields = HashBiMap.create();
    protected Multimap<FFunctionIdentifier, FFunction> functions = ArrayListMultimap.create();

    public FType(FTypeIdentifier identifier) {
        this.identifier = identifier;
    }

    @Override
    public FTypeIdentifier getIdentifier () {
        return identifier;
    }

    public BiMap<FIdentifier, FField> getInstanceFields() {
        return instanceFields;
    }

    public BiMap<FIdentifier, FField> getStaticFields() {
        return staticFields;
    }

    public Iterable<FField> getFields() {
        return Iterables.concat(getInstanceFields().values(), getStaticFields().values());
    }

    public Multimap<FFunctionIdentifier, FFunction> getFunctions() {
        return functions;
    }

    public Pair<FFunction, IntIntPair> resolveFunction (FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        return new FunctionResolver(identifier, arguments, typeInstantiation).resolve();
    }

    @Override
    public String toString() {
        return tS();
    }

    private class FunctionResolver {
        private FFunction bestFunction;
        private IntIntPair bestCosts;

        private FFunctionIdentifier identifier;
        private List<FExpression> arguments;
        private TypeInstantiation typeInstantiation;

        FunctionResolver(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) {
            this.identifier = identifier;
            this.arguments = arguments;
            this.typeInstantiation = typeInstantiation;
        }

        Pair<FFunction, IntIntPair> resolve() throws FunctionNotFound {
            for (FFunction f : getFunctions().get(identifier)) {
                try {
                    IntIntPair cost = f.castSignatureFrom(arguments, typeInstantiation);
                    updateCost(cost, f);
                } catch (FFunction.IncompatibleSignatures | IncompatibleTypes ignored) {}
            }

            if (bestFunction == null)
                throw new FunctionNotFound(identifier, Utils.typesFromExpressionList(arguments));
            return result();
        }

        private Pair<FFunction, IntIntPair> result() {
            return new Pair<>(bestFunction, bestCosts);
        }

        private void updateCost(IntIntPair newCosts, FFunction newFunction) {
            if (bestCosts == null) {
                bestCosts = newCosts;
                bestFunction = newFunction;
                return;
            }
            int res = newCosts.compareTo(bestCosts);
            if (res < 0) {
                bestCosts = newCosts;
                bestFunction = newFunction;
            } else if (res == 0) {
                bestFunction = null; //not obvious which function to call %TODO a far more descriptive error message then FNF
            }
        }
    }
}

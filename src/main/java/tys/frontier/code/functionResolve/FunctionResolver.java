package tys.frontier.code.functionResolve;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.UnboundExpression;
import tys.frontier.code.expression.UnparsedLambda;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.Constraints;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.google.common.collect.Iterables.getOnlyElement;
import static tys.frontier.code.typeInference.Variance.Covariant;
import static tys.frontier.util.Utils.typesFromExpressionList;
import static tys.frontier.util.Utils.zip;

public class FunctionResolver {

    public static class Result {
        public Signature signature;
        public ArgMatching argMatching;
        public FFunction getFunction() {return signature.getFunction();}
    }

    public static class Candidate {
        public final Result result = new Result();
        public int casts;
        public long costs;
    }

    private FIdentifier identifier;
    private List<FType> positionalArgs;
    private Map<FIdentifier, FType> keywordArgs;
    private FType returnType;
    private DefaultNamespace namespace;
    private boolean lhsResolve;
    private List<UnboundExpression> unboundExpressions;
    private List<FTypeVariable> unboundVars; // computed from unbounds

    private Candidate bestCandidate;

    public static Result resolve(FIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType, DefaultNamespace namespace, boolean lhsResove, List<UnboundExpression> unbounds) throws FunctionNotFound {
        return new FunctionResolver(identifier, positionalArgs, keywordArgs, returnType, namespace, lhsResove, unbounds).resolve();
    }

    private FunctionResolver(FIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType, DefaultNamespace namespace, boolean lhsResolve, List<UnboundExpression> unboundExpressions) {
        this.identifier = identifier;
        this.positionalArgs = positionalArgs;
        this.keywordArgs = keywordArgs;
        this.returnType = returnType;
        this.namespace = namespace;
        this.lhsResolve = lhsResolve;
        this.unboundExpressions = unboundExpressions;

        unboundVars = new ArrayList<>();
        for (UnboundExpression unbound : unboundExpressions) {
            unboundVars.addAll(unbound.getIn());
            unbound.getOut().ifPresent(out -> unboundVars.add(out));
        }
    }


    // TODO
    // I think I want some flags in the resolver that either allow or disallow returning an unbound call as result
    // also I want some flags to generate a useful error message that explicitly states where the resolve failed for each candidate
    private Result resolve() throws FunctionNotFound {
        List<Signature> candidates = namespace.getFunctions(lhsResolve).get(identifier);
        assert unboundExpressions.isEmpty() || candidates.size() == 1 : "overloaded higher order function!";
        for (final Signature s : candidates) {
            try {
                Candidate candidate = new Candidate();
                candidate.result.argMatching = ArgMatching.create(positionalArgs, keywordArgs, s.getParameters());

                var castsAndConstraints = computeCasts(candidate.result.argMatching, positionalArgs, keywordArgs, s.getParameters());

                // check for return Type if specified
                FType returnType = s.getType();
                if (this.returnType != null && this.returnType != returnType) {
                    castsAndConstraints.a.add(ImplicitTypeCast.create(this.returnType, returnType, Variance.Contravariant, castsAndConstraints.b));
                }
                candidate.casts = castsAndConstraints.a.size();
                candidate.costs = castsAndConstraints.a.stream().mapToLong(ImplicitTypeCast::getCost).sum();

                // fast path for perfect fit
                if (candidate.casts == 0) {
                    candidate.result.signature = s;
                    return candidate.result;
                }

                // first resolve the type parameters of the function
                TypeInstantiation instantiation = castsAndConstraints.b.resolveAllWithUntyped(s.getFunction().getParameters().values(), unboundVars);

                // then handle unbound expressions
                instantiation = resolveUnbounds(castsAndConstraints.b, instantiation);

                candidate.result.signature = s.getInstantiation(instantiation);
                updateCost(candidate);
            } catch (IncompatibleTypes | UnfulfillableConstraints | TooManyArguments | NotEnoughArguments ignored) {}
        }

        if (bestCandidate == null)
            throw new FunctionNotFound(identifier, this.positionalArgs, this.keywordArgs);

        return bestCandidate.result;
    }

    private static Pair<List<ImplicitTypeCast>, Constraints> computeCasts(ArgMatching argMatching, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, ImmutableList<FParameter> parameters) throws IncompatibleTypes, UnfulfillableConstraints {
        List<ImplicitTypeCast> casts = new ArrayList<>();
        Constraints constraints = new Constraints();

        List<FType> argList = argMatching.createArgList(positionalArgs, keywordArgs, t -> ((FTuple) t).getTypes());
        for (Pair<FType, FType> types : zip(argList, typesFromExpressionList(parameters)))
            if (types.a != null && types.a != types.b)
                casts.add(ImplicitTypeCast.create(types.a, types.b, Covariant, constraints));

        return new Pair<>(casts, constraints);
    }

    private TypeInstantiation resolveUnbounds(Constraints constraints, TypeInstantiation resolved) throws UnfulfillableConstraints {
        List<UnboundExpression> unboundExpressions = this.unboundExpressions;
        while (!unboundExpressions.isEmpty()) {
            Pair<List<UnboundExpression>, TypeInstantiation> pair = resolveUnboundsSingleIteration(unboundExpressions, constraints, resolved);
            if (pair.a.size() == unboundExpressions.size()) {
                throw new UnfulfillableConstraints(null); // TODO content
            }
            unboundExpressions = pair.a;
            resolved = pair.b;
        }
        return resolved;
    }

    private static Pair<List<UnboundExpression>, TypeInstantiation> resolveUnboundsSingleIteration(List<UnboundExpression> toBinds, Constraints constraints, TypeInstantiation resolved) {
        List<UnboundExpression> unbounds = new ArrayList<>();
        for (UnboundExpression toBind : toBinds) {
            try {
                resolved = bind(toBind, constraints, resolved);
            } catch (UnfulfillableConstraints | InvalidExpressionBinding ignored) {
                unbounds.add(toBind);
            }
        }

        return new Pair<>(unbounds, resolved);
    }

    private static TypeInstantiation bind(UnboundExpression expression, Constraints contraints, TypeInstantiation instantiation) throws UnfulfillableConstraints, InvalidExpressionBinding {
        Map<FTypeVariable, FType> resolveMap = new HashMap<>();

        for (FTypeVariable in : expression.getIn()) {
            FType newIn = contraints.resolve(in, instantiation);
            if (in == newIn)
                throw new UnfulfillableConstraints(contraints.get(in));
            resolveMap.put(in, newIn);
        }
        TypeInstantiation newBinding = TypeInstantiation.create(resolveMap);

        if (expression.getOut().isPresent()) {
            FTypeVariable out = expression.getOut().get();
            FType newOut;
            try {
                newOut = contraints.resolve(out, instantiation);
            } catch (UnfulfillableConstraints e) {
                assert contraints.get(out).size() == 1 && instantiation.getType(getOnlyElement(contraints.get(out)).getTarget()) == out;
                if (!(expression instanceof UnparsedLambda))
                    return Utils.NYI("binding something other than an unparsed lambda");
                FExpression bind = expression.bind(newBinding);
                assert bind.getType() instanceof FFunctionType;
                newOut = ((FFunctionType) bind.getType()).getOut();
            }

            newBinding = newBinding.with(out, newOut);
        }

        return instantiation.with(newBinding);
    }

    private void updateCost(Candidate newCandidate) {
        if (bestCandidate == null) {
            bestCandidate = newCandidate;
            return;
        }

        if (newCandidate.casts < bestCandidate.casts || (newCandidate.casts == bestCandidate.casts && newCandidate.costs < bestCandidate.costs)) {
            bestCandidate = newCandidate;
            return;
        }
        if (newCandidate.casts == bestCandidate.casts && newCandidate.costs == bestCandidate.costs) {
            bestCandidate = null; //not obvious which function to call TODO a far more descriptive error message then FNF
        }
    }

}

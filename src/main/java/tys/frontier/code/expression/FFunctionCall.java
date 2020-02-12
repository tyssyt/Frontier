package tys.frontier.code.expression;

import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ListMultimap;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.passes.GenericBaking;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping;

import java.util.*;

public class FFunctionCall implements FExpression {
    private boolean prepared;
    private Signature signature;
    private List<FExpression> arguments;
    private BitSet defaultArgs;
    private ArgMapping argMapping;

    private FFunctionCall(boolean prepared, Signature signature, List<FExpression> arguments, BitSet defaultArgs, ArgMapping argMapping) {
        this.prepared = prepared;
        this.signature = signature;
        this.arguments = arguments;
        this.defaultArgs = defaultArgs;
        this.argMapping = argMapping;
    }

    private void prepare() { //fills in default arguments
        if (prepared)
            return;
        prepared = true;

        FFunction function = signature.getFunction();
        if (function.isInstantiation()) {
            List<FParameter> params = function.getBaseR().getSignature().getParameters();
            List<FParameter> targetParams = signature.getParameters();

            //prepare varMap
            Map<FLocalVariable, FLocalVariable> varMap = new HashMap<>();
            for (Pair<FParameter, FParameter> pair : Utils.zip(params, targetParams))
                varMap.put(pair.a, pair.b);

            TypeInstantiation typeInstantiation = function.getTypeInstantiationToBase();
            for (int i = 0; i < arguments.size(); i++)
                if (arguments.get(i) == null) {
                    FExpression bake = GenericBaking.bake(params.get(i).getDefaultValue(), typeInstantiation, varMap); //TODO is there a smart way to figure out when we don't need baking?
                    try {
                        arguments.set(i, bake.typeCheck(targetParams.get(i).getType()));
                    } catch (IncompatibleTypes incompatibleTypes) {
                        Utils.cantHappen();
                    }
                }
        } else {
            List<FParameter> params = signature.getParameters();
            for (int i = 0; i < arguments.size(); i++)
                if (arguments.get(i) == null)
                    arguments.set(i, params.get(i).getDefaultValue());
        }
    }

    public static FFunctionCall create(Signature signature, List<FExpression> positionalArgs, ListMultimap<FIdentifier, FExpression> keywordArgs, ArgMapping argMapping) {
        List<FExpression> args = new ArrayList<>(positionalArgs);
        BitSet defaultArgs = new BitSet(signature.getParameters().size());
        ImmutableList<FParameter> parameters = signature.getParameters();
        for (int i = argMapping.getNumberOfParamsFilledWithPositionalArgs(); i < parameters.size(); i++) {
            FParameter p = parameters.get(i);
            List<FExpression> arg = keywordArgs.get(p.getIdentifier());
            if (arg.isEmpty()) {
                defaultArgs.set(i);
                args.add(null);
            } else
                args.addAll(arg); //adds null for default args
        }
        return new FFunctionCall(defaultArgs.isEmpty(), signature, args, defaultArgs, argMapping);
    }

    public static FFunctionCall createUnpreparedTrusted(Signature signature, List<FExpression> arguments, List<FType> paramTypes, BitSet defaultArgs) {
        try {
            ArgMapping argMapping = ArgMapping.createBasic(paramTypes, Utils.typesFromExpressionList(signature.getParameters()));
            return new FFunctionCall(defaultArgs.isEmpty(), signature, arguments, defaultArgs, argMapping);
        } catch (IncompatibleTypes | UnfulfillableConstraints error) {
            return Utils.cantHappen();
        }
    }

    public static FFunctionCall createTrusted(Signature signature, List<FExpression> arguments) {
        try {
            ArgMapping argMapping = ArgMapping.createBasic(Utils.typesFromExpressionList(arguments), Utils.typesFromExpressionList(signature.getParameters()));
            return new FFunctionCall(true, signature, arguments, new BitSet(arguments.size()), argMapping);
        } catch (IncompatibleTypes | UnfulfillableConstraints error) {
            return Utils.cantHappen();
        }
    }

    public Signature getSignature() {
        return signature;
    }

    public FFunction getFunction() {
        return signature.getFunction();
    }

    public List<? extends FExpression> getArguments() {
        prepare();
        return arguments;
    }

    public boolean isDefaultArg(int n) {
        return defaultArgs.get(n);
    }

    @Override
    public FType getType() {
        return signature.getType();
    }

    public ArgMapping getArgMapping() {
        return argMapping;
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        boolean visitDefaults = visitor.enterFunctionCall(this);
        if (visitDefaults)
            prepare();
        List<E> params = new ArrayList<>(this.arguments.size());
        for (int i = 0; i < arguments.size(); i++) {
            if (!visitDefaults && isDefaultArg(i))
                params.add(null);
            else
                params.add(arguments.get(i).accept(visitor));
        }
        return visitor.exitFunctionCall(this, params);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        prepare();
        return walker.visitFunctionCall(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(signature.getFunction().getMemberOf().getIdentifier());
        sb.append('.').append(signature.getFunction().getIdentifier()).append('(');
        return Joiner.on(',').appendTo(sb, arguments).append(')');
    }
    @Override
    public String toString() {
        return tS();
    }
}

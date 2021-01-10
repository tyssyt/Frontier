package tys.frontier.code.expression;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ListMultimap;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntList;
import it.unimi.dsi.fastutil.ints.IntLists;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Joiners;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

public class FFunctionCall extends FExpression {
    private Signature signature;
    private List<FExpression> arguments;
    private BitSet defaultArgs;
    private ArgMapping argMapping;

    private FFunctionCall(Position position, Signature signature, List<FExpression> arguments, BitSet defaultArgs, ArgMapping argMapping) {
        super(position);
        this.signature = signature;
        this.arguments = arguments;
        this.defaultArgs = defaultArgs;
        this.argMapping = argMapping;
    }

    public static FFunctionCall create(Position position, Signature signature, List<FExpression> positionalArgs, ListMultimap<FIdentifier, FExpression> keywordArgs, ArgMapping argMapping) {
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
        return new FFunctionCall(position, signature, args, defaultArgs, argMapping);
    }

    public static FFunctionCall createUnpreparedTrusted(Position position, Signature signature, List<FExpression> arguments, List<FType> paramTypes, BitSet defaultArgs) {
        try {
            ArgMapping argMapping = ArgMapping.createBasic(paramTypes, Utils.typesFromExpressionList(signature.getParameters()));
            return new FFunctionCall(position, signature, arguments, defaultArgs, argMapping);
        } catch (IncompatibleTypes | UnfulfillableConstraints error) {
            return Utils.cantHappen();
        }
    }

    public static FFunctionCall createTrusted(Position position, Signature signature, List<FExpression> arguments) {
        try {
            ArgMapping argMapping = ArgMapping.createBasic(Utils.typesFromExpressionList(arguments), Utils.typesFromExpressionList(signature.getParameters()));
            return new FFunctionCall(position, signature, arguments, new BitSet(arguments.size()), argMapping);
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

    public List<? extends FExpression> fillDefaultArgs(Signature signature) {
        List<FExpression> res = new ArrayList<>(arguments.size());
        for (int i = 0; i < arguments.size(); i++) {
            if (arguments.get(i) == null) {
                int j = argMapping.mapArgIndexToParamIndex(i);
                res.add(signature.getParameters().get(j).getDefaultValue());
            } else
                res.add(arguments.get(i));
        }
        return res;
    }

    public List<? extends FExpression> getArguments(boolean fillDefaultArgs) {
        if (fillDefaultArgs)
            return fillDefaultArgs(signature);
        else
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

    public IntList computeDefaultArgOrder() {
        if (defaultArgs.isEmpty())
            return IntLists.EMPTY_LIST;

        IntList res = new IntArrayList();
        ImmutableList<FParameter> parameters = signature.getParameters();

        boolean changed = true;
        while (changed) { //not the most efficient, but shouldn't need to be
            changed = false;
            boolean waiting = false;

            parameters:
            for (int i = 0; i < arguments.size(); i++) {
                if (!defaultArgs.get(i) || res.contains(i))
                    continue; //not a default arg or already in list
                for (FParameter dependency : parameters.get(argMapping.mapArgIndexToParamIndex(i)).getDefaultValueDependencies()) {
                    int j = argMapping.mapParamIndexToArgIndex(dependency.getIndex());
                    if (defaultArgs.get(j) && !res.contains(j)) {
                        waiting = true;
                        continue parameters; //still waiting for a dependency
                    }
                }
                //default arg but not waiting on dependencies
                res.add(i);
                changed = true;
                break;
            }

            if (!changed && waiting)
                //no progress was made but values are still waiting on dependencies, this is a cycle
                return Utils.NYI("cyclic default value dependency not broken by call"); //TODO
        }
        return res;
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        boolean visitDefaults = visitor.enterFunctionCall(this);
        List<E> params = new ArrayList<>(this.arguments.size());
        for (FExpression argument : getArguments(visitDefaults)) {
            if (argument == null)
                params.add(null);
            else
                params.add(argument.accept(visitor));
        }
        return visitor.exitFunctionCall(this, params);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitFunctionCall(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(signature.getFunction().getMemberOf().getIdentifier());
        sb.append('.').append(signature.getFunction().getIdentifier()).append('(');
        return Joiners.ON_COMMA_PACKED.appendTo(sb, arguments).append(')');
    }
}

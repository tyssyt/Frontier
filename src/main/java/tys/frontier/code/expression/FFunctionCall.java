package tys.frontier.code.expression;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Streams;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntList;
import it.unimi.dsi.fastutil.ints.IntLists;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Joiners;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;

import static tys.frontier.util.Utils.zip;

public class FFunctionCall extends FExpression {
    private Signature signature;
    private List<FExpression> arguments;
    private boolean hasDefaultArgs;

    private FFunctionCall(Position position, Signature signature, List<FExpression> arguments, boolean hasDefaultArgs) {
        super(position);
        this.signature = signature;
        this.arguments = arguments;
        this.hasDefaultArgs = hasDefaultArgs;
    }

    public static FFunctionCall create(Position position, Signature signature, List<FExpression> arguments) {
        ImmutableList<FParameter> parameters = signature.getParameters();
        assert parameters.size() == arguments.size();
        assert Streams.stream(zip(parameters, arguments))
                .filter(pair -> pair.b != null)
                .allMatch(pair -> pair.a.getType() == pair.b.getType());
        boolean hasDefaultArgs = false;
        for (FExpression a : arguments) {
            if (a == null) {
                hasDefaultArgs = true;
                break;
            }
        }
        return new FFunctionCall(position, signature, arguments, hasDefaultArgs);
    }

    public static FFunctionCall createForBaking(Position position, Signature signature, List<FExpression> arguments) {
        ImmutableList<FParameter> parameters = signature.getParameters();
        try {
            for (int i = 0; i < arguments.size(); i++) {
                FExpression arg = arguments.get(i);
                if (arg != null)
                    arguments.set(i, arg.typeCheck(parameters.get(i).getType()));
            }
        return create(position, signature, arguments);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public Signature getSignature() {
        return signature;
    }

    public FFunction getFunction() {
        return signature.getFunction();
    }

    public List<? extends FExpression> getArguments(boolean fillDefaultArgs) {
        if (fillDefaultArgs)
            return getArgsAndFillDefaultFrom(signature);
        else
            return arguments;
    }

    public List<? extends FExpression> getArgsAndFillDefaultFrom(Signature signature) {
        if (!hasDefaultArgs)
            return arguments;
        List<FExpression> res = new ArrayList<>(arguments.size());
        for (int i = 0; i < arguments.size(); i++) {
            if (arguments.get(i) == null) {
                res.add(signature.getParameters().get(i).getDefaultValue());
            } else
                res.add(arguments.get(i));
        }
        return res;
    }

    public boolean isDefaultArg(int n) {
        return arguments.get(n) == null;
    }

    @Override
    public FType getType() {
        return signature.getType();
    }

    public IntList computeDefaultArgOrder() {
        if (!hasDefaultArgs)
            return IntLists.EMPTY_LIST;

        IntList res = new IntArrayList();
        ImmutableList<FParameter> parameters = signature.getParameters();
        boolean changed = true;
        while (changed) { //not the most efficient, but shouldn't need to be
            changed = false;
            boolean waiting = false;

            parameters:
            for (int i = 0; i < parameters.size(); i++) {
                if (!isDefaultArg(i) || res.contains(i))
                    continue; //not a default arg or already in list
                for (FParameter dependency : parameters.get(i).getDefaultValueDependencies()) {
                    int j = dependency.getIndex();
                    if (isDefaultArg(j) && !res.contains(j)) {
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
        List<E> args = new ArrayList<>(this.arguments.size());
        for (FExpression argument : getArguments(visitDefaults)) {
            if (argument == null)
                args.add(null);
            else
                args.add(argument.accept(visitor));
        }
        return visitor.exitFunctionCall(this, args);
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

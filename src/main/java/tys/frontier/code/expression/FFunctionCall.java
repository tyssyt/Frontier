package tys.frontier.code.expression;

import com.google.common.base.Joiner;
import com.google.common.collect.ListMultimap;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.passes.GenericBaking;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ExpressionListToTypeListMapping;

import java.util.ArrayList;
import java.util.List;

public class FFunctionCall implements FExpression {
    private boolean prepared;
    private FFunction function;
    private List<FExpression> arguments;
    private ExpressionListToTypeListMapping argMapping;

    private FFunctionCall(boolean prepared, FFunction function, List<FExpression> arguments, ExpressionListToTypeListMapping argMapping) {
        this.prepared = prepared;
        this.function = function;
        this.arguments = arguments;
        this.argMapping = argMapping;
    }

    private void prepare() { //fills in default arguments
        if (prepared)
            return;
        prepared = true;

        if (function.isInstantiation()) {
            List<FParameter> params = function.getBaseR().getParams();
            List<FParameter> targetParams = function.getParams();
            TypeInstantiation typeInstantiation = function.getTypeInstantiationToBase();
            for (int i = 0; i < arguments.size(); i++)
                if (arguments.get(i) == null) {
                    FExpression bake = GenericBaking.bake(params.get(i).getDefaultValue(), typeInstantiation); //TODO is there a smart way to figure out when we don't need baking?
                    try {
                        arguments.set(i, bake.typeCheck(targetParams.get(i).getType()));
                    } catch (IncompatibleTypes incompatibleTypes) {
                        Utils.cantHappen();
                    }
                }
        } else {
            List<FParameter> params = function.getParams();
            for (int i = 0; i < arguments.size(); i++)
                if (arguments.get(i) == null)
                    arguments.set(i, params.get(i).getDefaultValue());
        }
    }

    public static FFunctionCall create(FFunction function, List<FExpression> positionalArgs, ListMultimap<FIdentifier, FExpression> keywordArgs, ExpressionListToTypeListMapping argMapping) {
        List<FExpression> args = new ArrayList<>(positionalArgs);
        boolean needsPrepare = false;
        for (int i=argMapping.getNumberOfParamsFilledWithPositionalArgs(); i < function.getParams().size(); i++) {
            FParameter p = function.getParams().get(i);
            List<FExpression> arg = keywordArgs.get(p.getIdentifier());
            if (arg.isEmpty()) {
                needsPrepare = true;
                args.add(null);
            } else
                args.addAll(arg); //adds null for default args
        }
        return new FFunctionCall(!needsPrepare, function, args, argMapping);
    }

    public static FFunctionCall createTrusted(FFunction function, List<FExpression> arguments) {
        try {
            ExpressionListToTypeListMapping argMapping = ExpressionListToTypeListMapping.createBasic(Utils.typesFromExpressionList(arguments), Utils.typesFromExpressionList(function.getParams()));
            return new FFunctionCall(false, function, arguments, argMapping);
        } catch (IncompatibleTypes | UnfulfillableConstraints error) {
            return Utils.cantHappen();
        }
    }

    public FFunction getFunction() {
        return function;
    }

    public void setFunction(FFunction function) {
        this.function = function;
    }

    public List<? extends FExpression> getArguments() {
        prepare();
        return arguments;
    }

    @Override
    public FType getType() {
        return function.getType();
    }

    public ExpressionListToTypeListMapping getArgMapping() {
        return argMapping;
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        prepare();
        visitor.enterFunctionCall(this);
        List<E> params = new ArrayList<>(this.arguments.size());
        for (FExpression arg : this.arguments)
            params.add(arg.accept(visitor));
        return visitor.exitFunctionCall(this, params);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        prepare();
        return walker.visitFunctionCall(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(function.getMemberOf().getIdentifier());
        sb.append('.').append(function.getIdentifier()).append('(');
        return Joiner.on(',').appendTo(sb, arguments).append(')');
    }
    @Override
    public String toString() {
        return tS();
    }
}

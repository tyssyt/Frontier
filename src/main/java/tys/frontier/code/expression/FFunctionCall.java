package tys.frontier.code.expression;

import com.google.common.base.Joiner;
import tys.frontier.code.FFunction;
import tys.frontier.code.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;

public class FFunctionCall implements FExpression {
    private FFunction function;
    private List<FExpression> arguments;

    private FFunctionCall(FFunction function, List<FExpression> arguments) throws IncompatibleTypes {
        this.function = function;
        this.arguments = arguments;
        checkTypes();
        function.addCall(this);
    }

    public static FFunctionCall create(FFunction function, List<FExpression> arguments) throws IncompatibleTypes {
        return new FFunctionCall(function, arguments);
    }
    public static FFunctionCall createTrusted(FFunction function, List<FExpression> arguments) {
        try {
            return create(function, arguments);
        } catch (IncompatibleTypes incompatibleTypes) {
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
        return arguments;
    }

    @Override
    public FType getType() {
        return function.getType();
    }

    private void checkTypes() throws IncompatibleTypes {
        List<FType> paramTypes = function.getSignature().getAllParamTypes();
        for (int i = 0; i < arguments.size(); i++) {
            arguments.set(i, arguments.get(i).typeCheck(paramTypes.get(i)));
        }
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterFunctionCall(this);
        List<E> params = new ArrayList<>(this.arguments.size());
        for (FExpression arg : this.arguments)
            params.add(arg.accept(visitor));
        return visitor.exitFunctionCall(this, params);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitFunctionCall(this);
    }

    @Override
    @SuppressWarnings("ResultOfMethodCallIgnored")
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

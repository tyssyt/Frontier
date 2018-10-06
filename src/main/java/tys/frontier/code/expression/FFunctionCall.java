package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class FFunctionCall implements FExpression, HasInstanceObject {
    private FExpression object; //null if the function is static
    private FFunction function;
    private List<FExpression> arguments;

    private FFunctionCall(FExpression object, FFunction function, List<FExpression> arguments) throws IncompatibleTypes {
        this.object = object;
        this.function = function;
        this.arguments = arguments;
        checkTypes();
        function.addCall(this);
    }

    public static FFunctionCall createInstance(FExpression object, FFunction function, List<FExpression> arguments) throws IncompatibleTypes {
        assert (!function.isStatic());
        return new FFunctionCall(object, function, arguments);
    }
    public static FFunctionCall createInstanceTrusted(FExpression object, FFunction function, List<FExpression> arguments) {
        try {
            return createInstance(object, function, arguments);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public static FFunctionCall createStatic(FFunction function, List<FExpression> arguments) throws IncompatibleTypes {
        assert (function.isStatic());
        return new FFunctionCall(null, function, arguments);
    }
    public static FFunctionCall createStaticTrusted(FFunction function, List<FExpression> arguments) {
        try {
            return createStatic(function, arguments);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    @Override
    public HasInstanceObject copy() {
        return Utils.cantHappen();
    }

    @Override
    public FExpression getObject() {
        return object;
    }

    @Override
    public void setObject(FExpression object) {
        this.object = object;
    }

    public FFunction getFunction() {
        return function;
    }

    public List<? extends FExpression> getArguments() {
        return arguments;
    }

    @Override
    public FClass getType() {
        return function.getType();
    }

    private void checkTypes() throws IncompatibleTypes {
        if (object != null)
            object = object.typeCheck(function.getMemberOf());
        List<FClass> paramTypes = function.getSignature().getAllParamTypes();
        for (int i = 0; i < arguments.size(); i++) {
            arguments.set(i, arguments.get(i).typeCheck(paramTypes.get(i)));
        }
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterFunctionCall(this);
        E object = this.object == null ? null : this.object.accept(visitor);
        List<E> params = new ArrayList<>(this.arguments.size());
        for (FExpression arg : this.arguments)
            params.add(arg.accept(visitor));
        return visitor.exitFunctionCall(this, object, params);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitFunctionCall(this);
    }

    @Override
    @SuppressWarnings("ResultOfMethodCallIgnored")
    public StringBuilder toString(StringBuilder sb) {
        if (object == null)
            sb.append(function.getMemberOf().getIdentifier());
        else
            object.toString(sb);
        sb.append('.').append(function.getIdentifier()).append('(');
        Iterator<? extends FExpression> it = arguments.iterator();
        if (it.hasNext()) {
            it.next().toString(sb);
            while (it.hasNext())
                it.next().toString(sb.append(", "));
        }
        return sb.append(')');
    }
    @Override
    public String toString() {
        return tS();
    }
}

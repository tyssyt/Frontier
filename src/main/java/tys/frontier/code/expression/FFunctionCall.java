package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class FFunctionCall implements FExpression, NeedsTypeCheck {
    private FExpression object; //null if the function is static
    private FFunction function;
    private List<FExpression> arguments;

    public FFunctionCall(FExpression object, FFunction function, List<FExpression> arguments) {
        assert (!function.isStatic());
        this.object = object;
        this.function = function;
        this.arguments = arguments;
    }

    public FFunctionCall(FFunction function, List<FExpression> arguments) {
        assert (function.isStatic());
        this.function = function;
        this.arguments = arguments;
    }

    public FExpression getObject() {
        return object;
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

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (object != null && object.getType() != function.getClazz())
            object = new FImplicitCast(function.getClazz(), object);
        List<FClass> paramTypes = function.getSignature().getParamTypes();
        for (int i = 0; i < arguments.size(); i++) {
            if (arguments.get(i).getType() != paramTypes.get(i))
                arguments.set(i, new FImplicitCast(paramTypes.get(i), arguments.get(i)));
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
    public StringBuilder toString(StringBuilder sb) {
        if (object == null)
            sb.append(function.getClazz().getIdentifier());
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

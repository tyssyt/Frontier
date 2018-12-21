package tys.frontier.code.expression;

import com.google.common.base.Joiner;
import tys.frontier.code.FType;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.List;

public class DynamicFunctionCall implements FExpression {
    private FExpression function;
    private List<FExpression> arguments;

    private DynamicFunctionCall(FExpression function, List<FExpression> arguments) throws IncompatibleTypes {
        this.function = function;
        this.arguments = arguments;
        checkTypes();
    }

    public static DynamicFunctionCall create(FExpression function, List<FExpression> arguments) throws IncompatibleTypes {
        return new DynamicFunctionCall(function, arguments);
    }

    public static DynamicFunctionCall createTrusted(FExpression function, List<FExpression> arguments) {
        try {
            return create(function, arguments);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public FExpression getFunction() {
        return function;
    }

    public List<FExpression> getArguments() {
        return arguments;
    }

    @Override
    public FType getType() {
        return ((FFunctionType) function.getType()).getOut();
    }

    private void checkTypes() throws IncompatibleTypes {
        if (!(function.getType() instanceof FFunctionType))
            throw new IncompatibleTypes(FFunctionType.from(Utils.typesFromExpressionList(arguments), FVoid.INSTANCE), function.getType());
        FFunctionType type = (FFunctionType) function.getType();
        List<FType> params = type.getIn();
        if (arguments.size() != params.size()) //TODO consider default args
            throw new IncompatibleTypes(FFunctionType.from(Utils.typesFromExpressionList(arguments), type.getOut()), function.getType());
        for (int i = 0; i < arguments.size(); i++) {
            arguments.set(i, arguments.get(i).typeCheck(params.get(i)));
        }
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return null; //TODO
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitDynamicFunctionCall(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        function.toString(sb).append('(');
        return Joiner.on(',').appendTo(sb, arguments).append(')');
    }

    @Override
    public String toString() {
        return tS();
    }
}

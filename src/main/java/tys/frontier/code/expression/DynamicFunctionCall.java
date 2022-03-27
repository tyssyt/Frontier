package tys.frontier.code.expression;

import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Joiners;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;

public class DynamicFunctionCall extends FExpression {
    private FExpression function;
    private List<FExpression> arguments;

    private DynamicFunctionCall(Position position, FExpression function, List<FExpression> arguments) throws IncompatibleTypes {
        super(position);
        this.function = function;
        this.arguments = arguments;
        checkTypes();
    }

    public static DynamicFunctionCall create(Position position, FExpression function, List<FExpression> arguments) throws IncompatibleTypes {
        return new DynamicFunctionCall(position, function, arguments);
    }

    public static DynamicFunctionCall createTrusted(Position position, FExpression function, List<FExpression> arguments) {
        try {
            return create(position, function, arguments);
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
        if (!(function.getType() instanceof FFunctionType)) {
            FType argType = FTuple.fromExpressionList(arguments);
            throw new IncompatibleTypes(getPosition(), FFunctionType.from(argType, FTuple.VOID), function.getType());
        }

        if (function instanceof UnboundExpression) {
            Utils.NYI("calling an unbound expression directly"); //TODO
        }

        FType type = ((FFunctionType) function.getType()).getIn();
        List<FType> params = FTuple.unpackType(type);

        if (arguments.size() != params.size()) { //TODO consider default args
            FType argType = FTuple.fromExpressionList(arguments);
            throw new IncompatibleTypes(getPosition(), FFunctionType.from(argType, FTuple.VOID), function.getType());
        }
        for (int i = 0; i < arguments.size(); i++) {
            arguments.set(i, arguments.get(i).typeCheck(params.get(i)));
        }
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterDynamicFunctionCall(this);
        E function = this.function.accept(visitor);
        List<E> args = new ArrayList<>(this.arguments.size());
        for (FExpression arg : this.arguments)
            args.add(arg.accept(visitor));
        return visitor.exitDynamicFunctionCall(this, function, args);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitDynamicFunctionCall(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        function.toString(sb).append('(');
        return Joiners.ON_COMMA_PACKED.appendTo(sb, arguments).append(')');
    }
}

package tys.frontier.code.statement;

import com.google.common.base.Joiner;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.NotEnoughArguments;
import tys.frontier.parser.syntaxErrors.TooManyArguments;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static tys.frontier.util.Utils.mutableSingletonList;

public class FReturn extends FStatement {

    private List<FExpression> expressions;
    private ArgMapping argMapping;
    private FFunction function;

    private FReturn(Position position, List<FExpression> expressions, FFunction function) throws IncompatibleTypes, TooManyArguments, NotEnoughArguments, UnfulfillableConstraints {
        super(position);
        this.expressions = expressions;
        this.function = function;
        this.argMapping = ArgMapping.createCasted(
                Utils.typesFromExpressionList(expressions),
                function.getType() == FTuple.VOID ? Collections.emptyList() : Collections.singletonList(function.getType()));
    }

    public static FReturn create(Position position, List<FExpression> expressions, FFunction function) throws IncompatibleTypes, TooManyArguments, NotEnoughArguments, UnfulfillableConstraints {
        return new FReturn(position, expressions, function);
    }
    public static FReturn createTrusted(Position position, List<FExpression> expressions, FFunction function) {
        try {
            return create(position, expressions, function);
        } catch (IncompatibleTypes | TooManyArguments | NotEnoughArguments | UnfulfillableConstraints e) {
            return Utils.cantHappen();
        }
    }
    public static FReturn createTrusted(Position position, FExpression expression, FFunction function) {
        return createTrusted(position, mutableSingletonList(expression), function);
    }

    public List<FExpression> getExpressions() {
        return expressions;
    }

    public ArgMapping getArgMapping() {
        return argMapping;
    }

    public FFunction getFunction() {
        return function;
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return Optional.of(function);
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterReturn(this);
        List<E> expressions = new ArrayList<>(this.expressions.size());
        for (FExpression e : this.expressions)
            expressions.add(e.accept(visitor));
        return visitor.exitReturn(this, expressions);
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitReturn(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("return");
        sb.append(Joiner.on(", ").join(expressions));
        return sb.append(';');
    }
}
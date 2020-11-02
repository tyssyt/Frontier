package tys.frontier.code.statement;

import com.google.common.base.Joiner;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.type.FType;
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
import java.util.List;
import java.util.Optional;

import static java.util.Collections.singletonList;
import static tys.frontier.util.Utils.mutableSingletonList;

public class FAssignment extends FStatement {

    private List<FExpression> lhsExpressions;
    private List<FExpression> values;
    private ArgMapping argMapping;

    private FAssignment(Position position, List<FExpression> lhsExpressions, List<FExpression> values, ArgMapping argMapping) {
        super(position);
        this.lhsExpressions = lhsExpressions;
        this.values = values;
        this.argMapping = argMapping;

        for (FExpression e : lhsExpressions) {
            if (e instanceof FVariableExpression)
                ((FVariableExpression) e).setAccessType(FVariableExpression.AccessType.STORE);
        }
    }

    public static FAssignment create(Position position, List<FExpression> lhsExpressions, List<FExpression> values) throws IncompatibleTypes, TooManyArguments, NotEnoughArguments, UnfulfillableConstraints {
        List<FType> target = new ArrayList<>();
        for (FExpression e : lhsExpressions) {
            if (e instanceof FVariableExpression)
                target.add(e.getType());
            else if (e instanceof FFunctionCall)
                target.addAll(Utils.typesFromExpressionList(((FFunctionCall) e).getFunction().getLhsSignature().getAssignees()));
            else
                Utils.cantHappen();
        }
        ArgMapping argMap = ArgMapping.createCasted(Utils.typesFromExpressionList(values), target);
        return new FAssignment(position, lhsExpressions, values, argMap);
    }

    public static FAssignment createTrusted(Position position, List<FExpression> lhsExpressions, List<FExpression> values) {
        try {
            return create(position, lhsExpressions, values);
        } catch (IncompatibleTypes | TooManyArguments | NotEnoughArguments | UnfulfillableConstraints incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    //TODO @PositionForGeneratedCode
    public static FAssignment createDecl(FLocalVariable variable, FExpression value) {
        try {
            ArgMapping argMapping = ArgMapping.createBasic(singletonList(value.getType()), singletonList(variable.getType()));
            return new FAssignment(null, singletonList(new FVarDeclaration(null, variable)), mutableSingletonList(value), argMapping);
        } catch (IncompatibleTypes | UnfulfillableConstraints incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public List<FExpression> getLhsExpressions() {
        return lhsExpressions;
    }

    public List<FExpression> getValues() {
        return values;
    }

    public ArgMapping getArgMapping() {
        return argMapping;
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return Optional.empty();
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterVarAssignment(this);
        List<E> lhsExpressions = new ArrayList<>(this.lhsExpressions.size());
        for (FExpression v : this.lhsExpressions)
            lhsExpressions.add(v.accept(visitor));
        List<E> values = new ArrayList<>(this.values.size());
        for (FExpression v : this.values)
            values.add(v.accept(visitor));
        return visitor.exitVarAssignment(this, lhsExpressions, values);
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitVarAssignment(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(Joiner.on(", ").join(lhsExpressions));
        sb.append(" = ");
        sb.append(Joiner.on(", ").join(values));
        return sb.append(';');
    }
}

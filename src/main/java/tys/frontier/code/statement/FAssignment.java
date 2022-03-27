package tys.frontier.code.statement;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.expression.Pack;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.NotEnoughArguments;
import tys.frontier.parser.syntaxErrors.TooManyArguments;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Joiners;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static tys.frontier.util.Utils.typesFromExpressionList;
import static tys.frontier.util.Utils.zip;

public class FAssignment extends FStatement {

    private List<FExpression> lhsExpressions;
    private List<FExpression> values;

    private FAssignment(Position position, List<FExpression> lhsExpressions, List<FExpression> values) {
        super(position);
        assert values.size() == lhsExpressions.size();
        this.lhsExpressions = lhsExpressions;
        this.values = values;

        for (FExpression e : lhsExpressions) {
            if (e instanceof FVariableExpression)
                ((FVariableExpression) e).setAccessType(FVariableExpression.AccessType.STORE);
            if (e instanceof Pack)
                for (FExpression inner : ((Pack) e).getExpressions())
                    if (inner instanceof FVariableExpression)
                        ((FVariableExpression) inner).setAccessType(FVariableExpression.AccessType.STORE);
        }
    }

    public static FAssignment create(Position position, List<FExpression> lhsExpressions, List<FExpression> values) throws IncompatibleTypes, TooManyArguments, NotEnoughArguments, UnfulfillableConstraints {
        List<FExpression> casted = new ArrayList<>(values.size());
        for (Pair<FExpression, FExpression> pair : zip(values, lhsExpressions))
            casted.add(pair.a.typeCheck(getAssignType(pair.b)));
        return new FAssignment(position, lhsExpressions, casted);
    }

    private static FType getAssignType(FExpression e) {
        if (e instanceof FVariableExpression)
            return e.getType();
        else if (e instanceof Pack)
            return e.getType();
        else if (e instanceof FFunctionCall)
            return FTuple.from(typesFromExpressionList(((FFunctionCall) e).getFunction().getLhsSignature().getAssignees()));
        else
            return Utils.cantHappen();
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
            value = value.typeCheck(variable.getType());
            return new FAssignment(null, List.of(new FVarDeclaration(variable)), List.of(value));
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public List<FExpression> getLhsExpressions() {
        return lhsExpressions;
    }

    public List<FExpression> getValues() {
        return values;
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
        Joiners.ON_COMMA.appendTo(sb, lhsExpressions).append(" = ");
        return Joiners.ON_COMMA.appendTo(sb, values).append(';');
    }
}

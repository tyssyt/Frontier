package tys.frontier.code.statement;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

import java.util.Arrays;
import java.util.Optional;

import static com.google.common.collect.ImmutableMap.toImmutableMap;

public class FVarAssignment implements FStatement, NeedsTypeCheck {

    private FVariableExpression variableExpression;
    private Operator operator;
    private FExpression value;

    public FVarAssignment(FVariableExpression variable, Operator operator, FExpression value) {
        this.variableExpression = variable;
        this.operator = operator;
        this.value = value;
        variable.setAccessType(FVariableExpression.AccessType.STORE);
    }

    public FVariableExpression getVariableExpression() {
        return variableExpression;
    }

    public Operator getOperator() {
        return operator;
    }

    public FExpression getValue() {
        return value;
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return Optional.empty();
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterVarAssignment(this);
        return visitor.exitVarAssignment(this, variableExpression.accept(visitor), value.accept(visitor));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitVarAssignment(this);
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (variableExpression.getType() != value.getType())
            throw new IncompatibleTypes(variableExpression.getType(), value.getType());
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        variableExpression.toString(sb).append(' ').append(operator).append(' ');
        return value.toString(sb).append(';');
    }
    @Override
    public String toString() {
        return tS();
    }

    public enum Operator { //TODO type restrictions of operators
        ASSIGN("="),
        ADD_ASSIGN("+="),
        SUB_ASSIGN("-="),
        MUL_ASSIGN("*="),
        DIV_ASSIGN("/="),
        AND_ASSIGN("&="),
        OR_ASSIGN("|="),
        XOR_ASSIGN("^="),
        MOD_ASSIGN("%=");

        private static ImmutableMap<String, Operator> stringMap =
                Arrays.stream(values()).collect(toImmutableMap(o -> o.stringRepresentation, o -> o));

        public final String stringRepresentation;

        Operator(String s) {
            stringRepresentation = s;
        }

        public static Operator fromString (String string) {
            return stringMap.get(string);
        }

        @Override
        public String toString() {
            return stringRepresentation;
        }
    }
}

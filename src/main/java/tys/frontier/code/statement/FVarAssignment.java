package tys.frontier.code.statement;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import tys.frontier.code.FClass;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.function.operator.FBinaryOperator;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.Arrays;
import java.util.Optional;

import static com.google.common.collect.ImmutableMap.toImmutableMap;
import static tys.frontier.code.function.operator.FBinaryOperator.Arith.*;

public class FVarAssignment implements FStatement {

    private FVariableExpression variableExpression;
    private Operator operator;
    private FExpression value;

    private FVarAssignment(FVariableExpression variable, Operator operator, FExpression value) throws IncompatibleTypes {
        this.variableExpression = variable;
        this.operator = operator;
        this.value = value;
        variable.setAccessType(FVariableExpression.AccessType.STORE);
        checkTypes();
    }

    public static FVarAssignment create(FVariableExpression variable, Operator operator, FExpression value) throws IncompatibleTypes {
        return new FVarAssignment(variable, operator, value);
    }
    public static FVarAssignment createTrusted(FVariableExpression variable, Operator operator, FExpression value) {
        try {
            return create(variable, operator, value);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
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

    private void checkTypes() throws IncompatibleTypes {
        value = value.typeCheck(variableExpression.getType());
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
        ASSIGN("=", null),
        ADD_ASSIGN("+=", PLUS.identifier),
        SUB_ASSIGN("-=", MINUS.identifier),
        MUL_ASSIGN("*=", TIMES.identifier),
        DIV_ASSIGN("/=", DIVIDED.identifier),
        AND_ASSIGN("&=", AND.identifier),
        OR_ASSIGN("|=", OR.identifier),
        XOR_ASSIGN("^=", XOR.identifier),
        MOD_ASSIGN("%=", MODULO.identifier);

        private static ImmutableMap<String, Operator> stringMap =
                Arrays.stream(values()).collect(toImmutableMap(o -> o.stringRepresentation, o -> o));

        public final String stringRepresentation;
        public final FFunctionIdentifier identifier;

        Operator(String s, FFunctionIdentifier id) {
            stringRepresentation = s;
            identifier = id;
        }

        public FBinaryOperator getOperator(FClass fClass) {
            return (FBinaryOperator) Iterables.getOnlyElement(fClass.getFunctions().get(identifier));
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

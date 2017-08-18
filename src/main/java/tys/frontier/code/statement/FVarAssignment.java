package tys.frontier.code.statement;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.FVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

import java.util.Arrays;

import static com.google.common.collect.ImmutableMap.toImmutableMap;

public class FVarAssignment implements FStatement, NeedsTypeCheck {

    private FVariable variable;
    private Operator operator;
    private FExpression value;

    public FVarAssignment(FVariable variable, Operator operator, FExpression value) {
        this.variable = variable;
        this.operator = operator;
        this.value = value;
    }

    public FVariable getVariable() {
        return variable;
    }

    public Operator getOperator() {
        return operator;
    }

    public FExpression getValue() {
        return value;
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (value.getType() != value.getType())
            throw new IncompatibleTypes(variable.getType(), value.getType());
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

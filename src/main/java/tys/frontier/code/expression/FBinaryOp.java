package tys.frontier.code.expression;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.FClass;

import java.util.Arrays;

import static com.google.common.collect.ImmutableMap.toImmutableMap;

public class FBinaryOp implements FExpression {

    private FExpression first;
    private FExpression second;
    private Operator operator;
    public FBinaryOp(FExpression first, FExpression second, Operator operator) {
        this.first = first;
        this.second = second;
        this.operator = operator;
    }

    @Override
    public FClass getType() {
        return null; //TODO
    }

    public enum Operator { //TODO type restrictions of operators
        EQ("=="),
        NE("!="),
        AND("&&"),
        OR("||"),
        XOR("^"),
        LE("<="),
        GE("<="),
        LT("<"),
        GT(">"),
        MUL("*"),
        DIV("/"),
        MOD("%"),
        ADD("+"),
        SUB("-");

        public final String stringRepresentation;

        private static ImmutableMap<String, Operator> stringMap =
                Arrays.stream(values()).collect(toImmutableMap(o -> o.stringRepresentation, o -> o));

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

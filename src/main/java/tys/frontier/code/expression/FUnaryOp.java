package tys.frontier.code.expression;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.FClass;
import tys.frontier.util.Pair;

import java.util.Arrays;

import static com.google.common.collect.ImmutableMap.toImmutableMap;

public class FUnaryOp implements FExpression {

    private final FExpression expression;
    private final Operator operator;

    public FUnaryOp(FExpression expression, Operator operator) {
        this.expression = expression;
        this.operator = operator;
    }

    @Override
    public FClass getType() {
        return null; //TODO
    }

    public enum Operator { //TODO type
        NOT("!", true),
        PRE_INCREMENT("++", true),
        PRE_DECREMENT("--", true),
        POST_INCREMENT("++", false),
        POST_ODECREMENT("--", false),
        PLUS("+", true),
        MINUS("-", true);

        private static ImmutableMap<Pair<String, Boolean>, Operator> stringMap =
                Arrays.stream(values()).collect(toImmutableMap(o -> new Pair<>(o.stringRepresentation, o.prefix), o -> o));

        public final String stringRepresentation;
        public boolean prefix;

        Operator(String s, boolean prefix) {
            stringRepresentation = s;
            this.prefix = prefix;
        }

        public static Operator fromString (String string, boolean prefix) {
            return stringMap.get(new Pair<>(string, prefix));
        }

        @Override
        public String toString() {
            return stringRepresentation;
        }
    }
}

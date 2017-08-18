package tys.frontier.code.expression;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.code.statement.NeedsTypeCheck;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Pair;

import java.util.Arrays;
import java.util.Set;

import static com.google.common.collect.ImmutableMap.toImmutableMap;

public class FUnaryOp implements FExpression, NeedsTypeCheck {

    private final FExpression expression;
    private final Operator operator;

    public FUnaryOp(FExpression expression, Operator operator) throws IncompatibleTypes {
        this.expression = expression;
        this.operator = operator;
        checkTypes();
    }

    @Override
    public FClass getType() {
        return expression.getType();
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (!operator.isValidType(expression.getType()))
            throw new IncompatibleTypes(operator.validTypes.iterator().next(), expression.getType());
    }

    public enum Operator { //TODO type
        NOT("!", true, FPredefinedClass.boolTypes),
        PRE_INCREMENT("++", true, FPredefinedClass.intTypes),
        PRE_DECREMENT("--", true, FPredefinedClass.intTypes),
        POST_INCREMENT("++", false, FPredefinedClass.intTypes),
        POST_DECREMENT("--", false, FPredefinedClass.intTypes),
        PLUS("+", true, FPredefinedClass.intTypes),
        MINUS("-", true, FPredefinedClass.intTypes);

        private static ImmutableMap<Pair<String, Boolean>, Operator> stringMap =
                Arrays.stream(values()).collect(toImmutableMap(o -> new Pair<>(o.stringRepresentation, o.prefix), o -> o));

        public final String stringRepresentation;
        public final boolean prefix;
        private Set<FClass> validTypes;

        Operator(String s, boolean prefix, Set<FClass> validTypes) {
            this.stringRepresentation = s;
            this.prefix = prefix;
            this.validTypes = validTypes;
        }

        public static Operator fromString (String string, boolean prefix) {
            return stringMap.get(new Pair<>(string, prefix));
        }

        public boolean isValidType(FClass type) {
            return validTypes.contains(type);
        }

        @Override
        public String toString() {
            return stringRepresentation;
        }
    }
}

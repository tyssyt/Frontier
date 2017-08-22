package tys.frontier.code.Operator;

import tys.frontier.code.identifier.FFunctionIdentifier;

import java.util.Arrays;
import java.util.Map;

import static java.util.stream.Collectors.toMap;

public class Operators {

    private Operators() {}

    public enum PreUnary {
        NOT(FFunctionIdentifier.NOT), //Sig: id + type of
        PLUS(FFunctionIdentifier.PLUS),
        MINUS(FFunctionIdentifier.MINUS);

        private static Map<String, PreUnary> stringMap = Arrays.stream(values()).collect(toMap(e -> e.identifier.name, e -> e));

        public final FFunctionIdentifier identifier;

        PreUnary(FFunctionIdentifier identifier) {
            this.identifier = identifier;
        }

        public static PreUnary fromString(String s) {
            return stringMap.get(s);
        }
    }

    public enum Binary {
        EQ(FFunctionIdentifier.EQUALS),
        NEQ(FFunctionIdentifier.NOT_EQUALS),
        AND(FFunctionIdentifier.AND),
        OR(FFunctionIdentifier.OR),
        XOR(FFunctionIdentifier.XOR),
        LT(FFunctionIdentifier.SMALLER),
        GT(FFunctionIdentifier.GREATER),
        LE(FFunctionIdentifier.SMALLER_EQUAL),
        GE(FFunctionIdentifier.GREATER_EQUAL);

        private static Map<String, Binary> stringMap = Arrays.stream(values()).collect(toMap(e -> e.identifier.name, e -> e));

        public final FFunctionIdentifier identifier;

        Binary(FFunctionIdentifier identifier) {
            this.identifier = identifier;
        }

        public static Binary fromString(String s) {
            return stringMap.get(s);
        }
    }

    //TODO this is all so messy but I really don't want to spend any more time until I have Operator overloading


}

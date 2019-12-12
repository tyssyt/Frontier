package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.type.FClass;
import tys.frontier.parser.antlr.FrontierLexer;

import static tys.frontier.code.function.operator.Operator.getParserToken;

public class FBinaryOperator extends FOperator {

    public enum Arith implements Operator {
        PLUS   (getParserToken(FrontierLexer.ADD),   new FFunctionIdentifier("+")),
        MINUS  (getParserToken(FrontierLexer.SUB),   new FFunctionIdentifier("-")),
        TIMES  (getParserToken(FrontierLexer.STAR),  new FFunctionIdentifier("*")),
        DIVIDED(getParserToken(FrontierLexer.SLASH), new FFunctionIdentifier("/")),
        MODULO (getParserToken(FrontierLexer.MOD),   new FFunctionIdentifier("%")),
        AND    (getParserToken(FrontierLexer.AAND),  new FFunctionIdentifier("&")),
        OR     (getParserToken(FrontierLexer.AOR),   new FFunctionIdentifier("|")),
        XOR    (getParserToken(FrontierLexer.XOR),   new FFunctionIdentifier("^"));

        private static final ImmutableList<FBinaryOperator.Arith> values = ImmutableList.copyOf(values());

        public final String parserToken;
        public final FFunctionIdentifier identifier;

        Arith(String parserToken, FFunctionIdentifier identifier) {
            this.parserToken = parserToken;
            this.identifier = identifier;
        }

        public FBinaryOperator create(FClass fClass, FClass returnType, FParameter first, FParameter second) {
            return new FBinaryOperator(identifier, fClass, returnType, first, second);
        }

        public FBinaryOperator createPredefined(FClass fClass) {
            return new FBinaryOperator(identifier, fClass, fClass,
                    FParameter.create(new FVariableIdentifier("first"), fClass, false),
                    FParameter.create(new FVariableIdentifier("second"), fClass, false)) {
                {predefined = true;}
            };
        }

        public static FBinaryOperator.Arith getFromParserToken(String parserToken) {
            for (FBinaryOperator.Arith value : values)
                if (value.parserToken.equals(parserToken))
                    return value;
            return null;
        }

        public FFunction getFunction(FClass fClass) {
            return Iterables.getOnlyElement(fClass.getFunctions().get(identifier));
        }

        @Override
        public FFunctionIdentifier getIdentifier() {
            return identifier;
        }

        @Override
        public boolean isUserDefinable() {
            return true;
        }
    }


    public enum Bool implements Operator  {
        AND                 (getParserToken(FrontierLexer.AND),                new FFunctionIdentifier("&&")),
        OR                  (getParserToken(FrontierLexer.OR),                 new FFunctionIdentifier("||")),
        EQUALS              (getParserToken(FrontierLexer.EQUAL),              new FFunctionIdentifier("==")),
        EQUALS_ID           (getParserToken(FrontierLexer.EQUAL_ID),           new FFunctionIdentifier("=*=")),
        EQUALS_CONTAINER    (getParserToken(FrontierLexer.EQUAL_CONTAINER),    new FFunctionIdentifier("=[]=")),
        NOT_EQUALS          (getParserToken(FrontierLexer.NOTEQUAL),           new FFunctionIdentifier("=!=")),
        NOT_EQUALS_ID       (getParserToken(FrontierLexer.NOTEQUAL_ID),        new FFunctionIdentifier("=!*=")),
        NOT_EQUALS_CONTAINER(getParserToken(FrontierLexer.NOTEQUAL_CONTAINER), new FFunctionIdentifier("=![]=")),
        LESS                (getParserToken(FrontierLexer.LT),                 new FFunctionIdentifier("<")),
        GREATER             (getParserToken(FrontierLexer.GT),                 new FFunctionIdentifier(">")),
        LESS_EQUAL          (getParserToken(FrontierLexer.LE),                 new FFunctionIdentifier("<=")),
        GREATER_EQUAL       (getParserToken(FrontierLexer.LT),                 new FFunctionIdentifier(">="));

        public final String parserToken;
        private static final ImmutableList<FBinaryOperator.Bool> values = ImmutableList.copyOf(values());

        public final FFunctionIdentifier identifier;

        Bool(String parserToken, FFunctionIdentifier identifier) {
            this.parserToken = parserToken;
            this.identifier = identifier;
        }

        public FBinaryOperator create(FClass fClass, FParameter first, FParameter second) {
            return new FBinaryOperator(identifier, fClass, FBool.INSTANCE, first, second);
        }

        public FBinaryOperator createPredefined(FClass fClass) {
            return new FBinaryOperator(identifier, fClass, FBool.INSTANCE,
                    FParameter.create(new FVariableIdentifier("first"), fClass, false),
                    FParameter.create(new FVariableIdentifier("second"), fClass, false)) {
                {predefined = true;}
            };
        }

        public static FBinaryOperator.Bool getFromParserToken(String parserToken) {
            for (FBinaryOperator.Bool value : values)
                if (value.parserToken.equals(parserToken))
                    return value;
            return null;
        }

        public FFunction getFunction(FClass fClass) {
            return Iterables.getOnlyElement(fClass.getFunctions().get(identifier));
        }

        @Override
        public FFunctionIdentifier getIdentifier() {
            return identifier;
        }

        @Override
        public boolean isUserDefinable() {
            switch (this) {
                case EQUALS_ID: case NOT_EQUALS:
                    return false;
                default:
                    return true;
            }
        }
    }

    private FBinaryOperator(FFunctionIdentifier identifier, FClass fClass, FClass returnType, FParameter first, FParameter second) {
        super(identifier, fClass, returnType, ImmutableList.of(first, second));
        assert fClass == first.getType();
        assert fClass == second.getType();
    }
}

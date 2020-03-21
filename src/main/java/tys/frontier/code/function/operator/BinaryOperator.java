package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FClass;
import tys.frontier.parser.antlr.FrontierLexer;

import java.util.Arrays;

import static com.google.common.collect.ImmutableMap.toImmutableMap;
import static java.util.Collections.emptyMap;
import static tys.frontier.code.function.operator.Operator.getParserToken;

public enum BinaryOperator implements Operator {
    //Arithmetic operators
    PLUS   (getParserToken(FrontierLexer.ADD),   new FIdentifier("+")),
    MINUS  (getParserToken(FrontierLexer.SUB),   new FIdentifier("-")),
    TIMES  (getParserToken(FrontierLexer.STAR),  new FIdentifier("*")),
    DIVIDED(getParserToken(FrontierLexer.SLASH), new FIdentifier("/")),
    MODULO (getParserToken(FrontierLexer.MOD),   new FIdentifier("%")),
    AAND    (getParserToken(FrontierLexer.AAND),  new FIdentifier("&")),
    AOR     (getParserToken(FrontierLexer.AOR),   new FIdentifier("|")),
    XOR    (getParserToken(FrontierLexer.XOR),   new FIdentifier("^")),

    //Boolean Operators
    AND                 (getParserToken(FrontierLexer.AND),                new FIdentifier("&&")),
    OR                  (getParserToken(FrontierLexer.OR),                 new FIdentifier("||")),
    EQUALS              (getParserToken(FrontierLexer.EQUAL),              new FIdentifier("==")),
    EQUALS_ID           (getParserToken(FrontierLexer.EQUAL_ID),           new FIdentifier("=*=")),
    EQUALS_CONTAINER    (getParserToken(FrontierLexer.EQUAL_CONTAINER),    new FIdentifier("=[]=")),
    NOT_EQUALS          (getParserToken(FrontierLexer.NOTEQUAL),           new FIdentifier("=!=")),
    NOT_EQUALS_ID       (getParserToken(FrontierLexer.NOTEQUAL_ID),        new FIdentifier("=!*=")),
    NOT_EQUALS_CONTAINER(getParserToken(FrontierLexer.NOTEQUAL_CONTAINER), new FIdentifier("=![]=")),
    LESS                (getParserToken(FrontierLexer.LT),                 new FIdentifier("<")),
    GREATER             (getParserToken(FrontierLexer.GT),                 new FIdentifier(">")),
    LESS_EQUAL          (getParserToken(FrontierLexer.LE),                 new FIdentifier("<=")),
    GREATER_EQUAL       (getParserToken(FrontierLexer.GE),                 new FIdentifier(">="));

    private static final ImmutableMap<String, BinaryOperator> parserTokenMap =
            Arrays.stream(values()).collect(toImmutableMap(o -> o.parserToken, o -> o));

    public final String parserToken;
    public final FIdentifier identifier;

    BinaryOperator(String parserToken, FIdentifier identifier) {
        this.parserToken = parserToken;
        this.identifier = identifier;
    }

    public static BinaryOperator getFromParserToken(String parserToken) {
        return parserTokenMap.get(parserToken);
    }

    @Override
    public FIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public boolean isUserDefinable() {
        switch (this) {
            case EQUALS_ID: case NOT_EQUALS_ID:
                return false;
            default:
                return true;
        }
    }

    public FFunction getFunction(DefaultNamespace namespace) { //TODO remove on binOp remake
        return Iterables.getOnlyElement(namespace.getFunctions(false).get(identifier)).getFunction();
    }

    public FFunction createPredefined(FClass memberOf, FClass second, FClass ret) {
        ImmutableList<FParameter> params = ImmutableList.of(
                FParameter.create(new FIdentifier("first"), memberOf, false),
                FParameter.create(new FIdentifier("second"), second, false)
        );
        return new FBaseFunction(identifier, memberOf.getNamespace(), memberOf.getVisibility(), false, ret, params, null, emptyMap()) {
            {predefined = true;}
        };
    }
}
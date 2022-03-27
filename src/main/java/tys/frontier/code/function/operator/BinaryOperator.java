package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

import java.util.Arrays;
import java.util.List;

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
        return switch (this) {
            case EQUALS_ID, NOT_EQUALS_ID, AND, OR -> false;
            default -> true;
        };
    }

    public Signature getFunctionTrusted(FType first, FType second) {
        try {
            return first.getNamespace().resolveFunction(identifier, Arrays.asList(first, second), emptyMap(), null, false, List.of()).signature;
        } catch (FunctionNotFound functionNotFound) {
            return Utils.cantHappen();
        }
    }

    public FFunction addPredefined(FClass fClass, FClass ret) throws SignatureCollision {
        DefaultNamespace namespace = fClass.getNamespace();
        FBaseFunction res = new FunctionBuilder(identifier, namespace)
                .setVisibility(namespace.getVisibility()).setPredefined(true).setParams(fClass, fClass).setReturnType(ret).build();
        namespace.addFunction(res);
        return res;
    }
}
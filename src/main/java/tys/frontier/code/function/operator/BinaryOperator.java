package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.InvalidOpenDeclaration;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

import java.util.Arrays;
import java.util.Optional;

import static com.google.common.collect.ImmutableMap.toImmutableMap;
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

    private static DefaultNamespace binOpNamespace;

    static { //init binOpNamespace
        FIdentifier p1Id = new FIdentifier("P1");
        FIdentifier p2Id = new FIdentifier("P2");
        FIdentifier rId = new FIdentifier("R");

        binOpNamespace =  new DefaultNamespace(new FIdentifier("!BinOps"), FVisibilityModifier.EXPORT, true);
        for (BinaryOperator binaryOperator : parserTokenMap.values()) {
            //add open function to binOp namespace
            FTypeVariable p1 = FTypeVariable.create(p1Id, true);
            FTypeVariable p2 = FTypeVariable.create(p2Id, true);
            FTypeVariable r = FTypeVariable.create(rId, true);

            FBaseFunction function = new FunctionBuilder(binaryOperator.identifier, binOpNamespace)
                    .setParams(p1, p2).setReturnType(r).setParameters(p1, p2, r).build();
            try {
                binOpNamespace.setOpen(function);
            } catch (InvalidOpenDeclaration invalidOpenDeclaration) {
                Utils.cantHappen();
            }
        }
    }


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

    public static DefaultNamespace sGetNamespace() {
        return binOpNamespace;
    }

    @Override
    public Optional<DefaultNamespace> getNamespace() {
        return Optional.of(binOpNamespace);
    }

    public Signature getFunction(FType first, FType second) throws FunctionNotFound {
        return binOpNamespace.hardResolveFunction(identifier, Arrays.asList(first, second), ImmutableListMultimap.of(), null, false).signature;
    }

    public Signature getFunctionTrusted(FType first, FType second) {
        try {
            return getFunction(first, second);
        } catch (FunctionNotFound functionNotFound) {
            return Utils.cantHappen();
        }
    }

    public FFunction addPredefined(FClass fClass, FClass ret) throws SignatureCollision {
        FBaseFunction res = new FunctionBuilder(identifier, binOpNamespace)
                .setVisibility(fClass.getVisibility()).setPredefined(true).setParams(fClass, fClass).setReturnType(ret).build();
        binOpNamespace.addFunction(res);
        return res;
    }

    public static void resetNamespace() {
        BinaryOperator.sGetNamespace().getFunctions(false).values()
                .removeIf( sig -> !sig.getFunction().isPredefined() || sig.getType() instanceof FOptional);
    }
}
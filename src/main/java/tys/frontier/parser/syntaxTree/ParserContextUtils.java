package tys.frontier.parser.syntaxTree;

import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.FClass;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FErrorIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.FBoolLiteral;
import tys.frontier.code.literal.FInteger32Literal;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.parser.FrontierParser;
import tys.frontier.parser.syntaxTree.syntaxErrors.ClassNotFound;
import tys.frontier.util.Pair;

import java.util.*;

public final class ParserContextUtils {

    private ParserContextUtils() {}

    public static FClass getClass (FrontierParser.ClassDeclarationContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        FClassIdentifier identifier = new FClassIdentifier(ctx.TypeIdentifier().getText());
        return new FClass(identifier, visibilityModifier);
    }

    public static FVisibilityModifier getVisibility (FrontierParser.VisibilityModifierContext ctx) {
        if (ctx == null)
            return FVisibilityModifier.NONE;
        switch (((TerminalNode)ctx.children.get(0)).getSymbol().getType()) {
            case FrontierParser.PUBLIC:
                return FVisibilityModifier.PUBLIC;
            case FrontierParser.PRIVATE:
                return FVisibilityModifier.PRIVATE;
        }
        return null;
    }

    public static boolean isStatic (FrontierParser.ModifierContext ctx) {
        return ctx !=  null;
    }

    public static FClass getPredefined (FrontierParser.PredefinedTypeContext ctx) {
        switch (((TerminalNode)ctx.children.get(0)).getSymbol().getType()) {
            case FrontierParser.BOOL:
                return FBool.INSTANCE;
            case FrontierParser.INT:
                return FInt.INSTANCE;
            case FrontierParser.INT32:
                return FInt32.INSTANCE;
            case FrontierParser.INT64:
                return FInt64.INSTANCE;
            case FrontierParser.FLOAT32:
                return FFloat32.INSTANCE;
            case FrontierParser.FLOAT64:
                return FFloat64.INSTANCE;
        }
        return null;
    }

    public static Pair<FClass, Optional<ClassNotFound>> getBasicType (FrontierParser.BasicTypeContext ctx, Map<FClassIdentifier, FClass> possibleTypes) {
        FClass type;
        Optional<ClassNotFound> e = Optional.empty();
        FrontierParser.PredefinedTypeContext c = ctx.predefinedType();
        if (c != null) {
            type = getPredefined(c);
        } else {
            FClassIdentifier identifier = new FClassIdentifier(ctx.TypeIdentifier().getText());
            FClass clazz = possibleTypes.get(identifier);
            if (clazz != null)
                type = clazz.getType();
            else {
                type = new FErrorClassType(new FErrorIdentifier(identifier));
                e = Optional.of(new ClassNotFound(identifier));
            }
        }
        return new Pair<>(type, e);
    }

    public static Pair<FClass, Optional<ClassNotFound>> getType (FrontierParser.TypeTypeContext ctx, Map<FClassIdentifier, FClass> possibleTypes) {
        Pair<FClass, Optional<ClassNotFound>> res = getBasicType(ctx.basicType(), possibleTypes);
        int arrayDepth = ctx.Array().size();
        if (arrayDepth > 0)
            res.a = FArray.getArrayFrom(res.a, arrayDepth);
        return res;
    }

    public static Pair<FLocalVariable, Optional<ClassNotFound>> getVariable (FrontierParser.TypedIdentifierContext ctx, Map<FClassIdentifier, FClass> possibleTypes) {
        Pair<FClass, Optional<ClassNotFound>> typeAndError = getType(ctx.typeType(), possibleTypes);
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        return new Pair<>(new FLocalVariable(identifier, typeAndError.a), typeAndError.b);
    }

    public static FLiteral getLiteral (FrontierParser.LiteralContext ctx) {
        ParseTree child = ctx.children.get(0);
        FLiteral res = null;
        if (child instanceof FrontierParser.BooleanLiteralContext) {
            if (((FrontierParser.BooleanLiteralContext) child).TRUE() != null)
                res = FBoolLiteral.TRUE;
            else
                res = FBoolLiteral.FALSE;
        }
        if (child instanceof TerminalNode) {
            Token token = ((TerminalNode) child).getSymbol();
            String text = token.getText();
            switch (token.getType()) {
                case FrontierParser.IntegerLiteral:
                    if (text.endsWith("L") || text.endsWith("l"))
                        throw new RuntimeException("no longs for now");
                    res = new FInteger32Literal(Integer.parseInt(text));
                    break;
                case FrontierParser.NULL:
                    res = FNull.INSTANCE;
                    break;
                case FrontierParser.FloatingPointLiteral:
                    throw new RuntimeException("no floating");
                case FrontierParser.CharacterLiteral:
                    throw new RuntimeException("char literals not yet implemented, you fool!");
                case FrontierParser.StringLiteral:
                    throw new RuntimeException("String literals not yet implemented, you fool!");
            }
        }
        return res;
    }

    //TODO its late, I'm tired, this can be done less ugly
    public static Pair<List<FLocalVariable>, List<ClassNotFound>> getParams (FrontierParser.FormalParametersContext ctx, Map<FClassIdentifier, FClass> possibleTypes) {
        List<FrontierParser.TypedIdentifierContext> cs = ctx.typedIdentifier();
        //because I feel like optimizing just this particular thing...
        if (cs.isEmpty())
            return new Pair<>(Collections.emptyList(), Collections.emptyList());
        List<FLocalVariable> res = new ArrayList<>(cs.size());
        List<ClassNotFound> errors = new ArrayList<>();
        for (FrontierParser.TypedIdentifierContext c : cs) {
            Pair<FLocalVariable, Optional<ClassNotFound>> varAndError = getVariable(c, possibleTypes);
            res.add(varAndError.a);
            varAndError.b.ifPresent(errors::add);
        }
        return new Pair<>(res, errors);
    }


}

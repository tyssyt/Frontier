package tys.frontier.parser.syntaxTree;

import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.*;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.*;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.PrivateInterface;
import tys.frontier.parser.syntaxErrors.TypeNotFound;
import tys.frontier.util.Utils;

import java.util.Map;

public final class ParserContextUtils {

    private ParserContextUtils() {}

    public static FClass getClass (FrontierParser.ClassDeclarationContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        FTypeIdentifier identifier = new FTypeIdentifier(ctx.TypeIdentifier().getText());
        return new FClass(identifier, visibilityModifier);
    }

    public static FInterface getInterface (FrontierParser.InterfaceDeclarationContext ctx) throws PrivateInterface {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        FTypeIdentifier identifier = new FTypeIdentifier(ctx.TypeIdentifier().getText());
        return new FInterface(identifier, visibilityModifier);
    }

    public static FVisibilityModifier getVisibility (FrontierParser.VisibilityModifierContext ctx) {
        if (ctx == null)
            return FVisibilityModifier.NONE;
        switch (((TerminalNode)ctx.children.get(0)).getSymbol().getType()) {
            case FrontierParser.EXPORT:
                return FVisibilityModifier.EXPORT;
            case FrontierParser.PRIVATE:
                return FVisibilityModifier.PRIVATE;
        }
        return null;
    }

    public static boolean isStatic (FrontierParser.ModifierContext ctx) {
        return ctx !=  null;
    }

    public static FPredefinedClass getPredefined (FrontierParser.PredefinedTypeContext ctx) {
        switch (((TerminalNode)ctx.children.get(0)).getSymbol().getType()) {
            case FrontierParser.BOOL:
                return FBool.INSTANCE;
            case FrontierParser.INT:
                return Utils.NYI("unbounded int type");
            case FrontierParser.CHAR:
                return FIntN._8;
            case FrontierParser.INT32:
                return FIntN._32;
            case FrontierParser.INT64:
                return FIntN._64;
            case FrontierParser.FLOAT32:
                return FFloat32.INSTANCE;
            case FrontierParser.FLOAT64:
                return FFloat64.INSTANCE;
            default:
                return Utils.NYI("Frontier type for: " + ((TerminalNode)ctx.children.get(0)).getSymbol().getText());
        }
    }

    public static FType getNonPredefined(String id, Map<FTypeIdentifier, FType> possibleTypes) throws TypeNotFound {
        FTypeIdentifier identifier = new FTypeIdentifier(id);
        FType type = possibleTypes.get(identifier);
        if (type==null) {
            throw new TypeNotFound(identifier);
        }
        return type;
    }

    public static FType getBasicType (FrontierParser.BasicTypeContext ctx, Map<FTypeIdentifier, FType> possibleTypes)
            throws TypeNotFound {
        FrontierParser.PredefinedTypeContext predefined = ctx.predefinedType();
        return predefined != null ? getPredefined(predefined) : getNonPredefined(ctx.TypeIdentifier().getText(), possibleTypes);
    }

    public static FType getType (FrontierParser.TypeTypeContext ctx, Map<FTypeIdentifier, FType> possibleTypes)
            throws TypeNotFound {
        FType res = getBasicType(ctx.basicType(), possibleTypes);
        int arrayDepth = ctx.Array().size();
        if (arrayDepth > 0)
            res = FArray.getArrayFrom(res, arrayDepth);
        return res;
    }

    public static FLocalVariable getVariable (FrontierParser.TypedIdentifierContext ctx, Map<FTypeIdentifier, FType> possibleTypes)
            throws TypeNotFound {
        FType type = getType(ctx.typeType(), possibleTypes);
        FVariableIdentifier identifier = new FVariableIdentifier((ctx.Identifier().getText()));
        return new FLocalVariable(identifier, type);
    }

    public static FParameter getParameter (FrontierParser.TypedIdentifierContext ctx, Map<FTypeIdentifier, FType> possibleTypes)
            throws TypeNotFound {
        FType type = getType(ctx.typeType(), possibleTypes);
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        return new FParameter(identifier, type);
    }

    public static FLiteral getLiteral (FrontierParser.LiteralContext ctx) { //TODO why do we have res instead of just return (look at once all literals are done)
        ParseTree child = ctx.getChild(0);
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
                        res = new FIntNLiteral(Long.parseLong(text.substring(0,text.length()-1)), 64, text);
                    else
                        res = new FIntNLiteral(Integer.parseInt(text), 32, text);
                    break;
                case FrontierParser.NULL:
                    res = FNull.INSTANCE;
                    break;
                case FrontierParser.FloatingPointLiteral:
                    return Utils.NYI("float literals");
                case FrontierParser.CharacterLiteral:
                    assert text.charAt(0) == '\'';
                    assert text.charAt(text.length()-1) == '\'';
                    if (text.length() == 3)
                        return new FCharLiteral(text.charAt(1));
                    else if (text.length() == 4) {
                        assert text.charAt(1) == '\\';
                        return new FCharLiteral(text.charAt(2));
                    } else
                        assert false;
                case FrontierParser.StringLiteral:
                    assert text.charAt(0) == '\"';
                    assert text.charAt(text.length()-1) == '\"';
                    return new FStringLiteral(text.substring(1,text.length()-1));
            }
        }
        return res;
    }
}

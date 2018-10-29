package tys.frontier.parser.syntaxTree;

import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.FClass;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.*;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.selector.Selector;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.TypeNotFound;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public final class ParserContextUtils {

    private ParserContextUtils() {}

    public static FClass getClass (FrontierParser.ClassDeclarationContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        FTypeIdentifier identifier = new FTypeIdentifier(ctx.TypeIdentifier().getText());
        return new FClass(identifier, visibilityModifier);
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
            case FrontierParser.TYPE:
                return FType.INSTANCE;
            default:
                return Utils.NYI("Frontier type for: " + ((TerminalNode)ctx.children.get(0)).getSymbol().getText());
        }
    }

    public static FClass getNonPredefined(String id, Map<FTypeIdentifier, FClass> possibleTypes) throws TypeNotFound {
        FTypeIdentifier identifier = new FTypeIdentifier(id);
        FClass type = possibleTypes.get(identifier);
        if (type==null) {
            throw new TypeNotFound(identifier);
        }
        return type;
    }

    public static Selector<FFunctionIdentifier> getNameSelector(FrontierParser.NameSelectorContext ctx) {
        if (ctx.STAR() != null && ctx.BACKSLASH() == null)
            return Selector.all();
        List<TerminalNode> nodes = ctx.Identifier();
        List<FFunctionIdentifier> res = new ArrayList<>(nodes.size());
        for (TerminalNode node : nodes) {
            res.add(new FFunctionIdentifier(node.getText()));
        }
        if (ctx.BACKSLASH() == null)
            return Selector.in(res);
        else
            return Selector.notIn(res);
    }

    public static FClass getBasicType (FrontierParser.BasicTypeContext ctx, Map<FTypeIdentifier, FClass> possibleTypes)
            throws TypeNotFound {
        FrontierParser.PredefinedTypeContext predefined = ctx.predefinedType();
        return predefined != null ? getPredefined(predefined) : getNonPredefined(ctx.TypeIdentifier().getText(), possibleTypes);
    }

    public static FClass getType (FrontierParser.TypeTypeContext ctx, Map<FTypeIdentifier, FClass> possibleTypes)
            throws TypeNotFound {
        FClass res;
        FrontierParser.BasicTypeContext basic = ctx.basicType();
        if (basic != null) {
            res = getBasicType(basic, possibleTypes);
        } else {
            res = getType(ctx.typeType(), possibleTypes);
            res = FArray.getArrayFrom(res);
        }
        if (ctx.QUESTION() != null) {
            res = FOptional.from(res);
        }
        return res;
    }

    public static FParameter getParameter (FrontierParser.FormalParameterContext ctx, Map<FTypeIdentifier, FClass> possibleTypes)
            throws TypeNotFound {
        FClass type = getType(ctx.typeType(), possibleTypes);
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
                    res = new FIntNLiteral(Long.parseLong(text), text);
                    break;
                case FrontierParser.NULL:
                    res = FNull.UNTYPED;
                    break;
                case FrontierParser.FloatingPointLiteral:
                    return Utils.NYI("float literals");
                case FrontierParser.StringLiteral:
                    assert text.charAt(0) == '\"';
                    assert text.charAt(text.length()-1) == '\"';
                    StringBuilder sb = new StringBuilder();
                    for (int i=1; i<text.length()-1; i++) {
                        char c = text.charAt(i);
                        if (c == '\\') {
                            i++;
                            c = FCharLiteral.escapeLiterals.get(text.charAt(i));
                            assert c != 0;
                        }
                        sb.append(c);
                    }
                    return new FStringLiteral(sb.toString());
            }
        }
        return res;
    }
}

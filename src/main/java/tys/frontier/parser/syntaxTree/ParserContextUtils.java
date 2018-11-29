package tys.frontier.parser.syntaxTree;

import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.*;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.selector.Selector;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.ParameterizedTypeVariable;
import tys.frontier.parser.syntaxErrors.TwiceDefinedLocalVariable;
import tys.frontier.parser.syntaxErrors.TypeNotFound;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public final class ParserContextUtils {

    private ParserContextUtils() {}

    public static FClass getClass (FrontierParser.ClassDeclarationContext ctx) throws TwiceDefinedLocalVariable {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        FTypeIdentifier identifier = new FTypeIdentifier(ctx.TypeIdentifier().getText());
        FrontierParser.TypeParametersContext c = ctx.typeParameters();
        if (c == null) {
            return new FClass(identifier, visibilityModifier);
        }
        Map<FTypeIdentifier, FTypeVariable> parameters = new LinkedHashMap<>();
        for (TerminalNode node : c.TypeIdentifier()) {
            FTypeIdentifier id = new FTypeIdentifier(node.getText());
            if (parameters.containsKey(id))
                throw new TwiceDefinedLocalVariable(id);
            FLocalVariable var = new FLocalVariable(id, FTypeType.INSTANCE);
            parameters.put(id, new FTypeVariable(var));
        }
        return new FClass(identifier, visibilityModifier, parameters);
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
                return FTypeType.INSTANCE;
            default:
                return Utils.NYI("Frontier type for: " + ((TerminalNode)ctx.children.get(0)).getSymbol().getText());
        }
    }

    public static FType getNonPredefined(String id, Function<FTypeIdentifier, FType> possibleTypes) throws TypeNotFound {
        FTypeIdentifier identifier = new FTypeIdentifier(id);
        FType type = possibleTypes.apply(identifier);
        if (type==null) {
            throw new TypeNotFound(identifier);
        }
        return type;
    }

    public static Selector<FFunctionIdentifier> getNameSelector(FrontierParser.NameSelectorContext ctx) {
        if (ctx.STAR() != null && ctx.BACKSLASH() == null)
            return Selector.all();
        List<TerminalNode> nodes = ctx.LCIdentifier();
        List<FFunctionIdentifier> res = new ArrayList<>(nodes.size());
        for (TerminalNode node : nodes) {
            res.add(new FFunctionIdentifier(node.getText()));
        }
        if (ctx.BACKSLASH() == null)
            return Selector.in(res);
        else
            return Selector.notIn(res);
    }

    public static FType getBasicType (FrontierParser.BasicTypeContext ctx, Function<FTypeIdentifier, FType> possibleTypes)
            throws TypeNotFound, ParameterizedTypeVariable, WrongNumberOfTypeArguments {
        FrontierParser.PredefinedTypeContext predefined = ctx.predefinedType();
        FType base = predefined != null ? getPredefined(predefined) : getNonPredefined(ctx.TypeIdentifier().getText(), possibleTypes);
        List<FType> parameters = new ArrayList<>();
        for (FrontierParser.TypeTypeContext c : ctx.typeType()) {
            parameters.add(getType(c, possibleTypes));
        }
        if (base instanceof FClass)
            return FInstantiatedClass.from((FClass) base, parameters);
        else if (parameters.size() != 0)
            throw new ParameterizedTypeVariable(null); //TODO
        else
            return base;
    }

    public static FType getType (FrontierParser.TypeTypeContext ctx, Function<FTypeIdentifier, FType> possibleTypes)
            throws TypeNotFound, ParameterizedTypeVariable, WrongNumberOfTypeArguments {
        FType res;
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

    public static FParameter getParameter (FrontierParser.FormalParameterContext ctx, Function<FTypeIdentifier, FType> possibleTypes)
            throws TypeNotFound, ParameterizedTypeVariable, WrongNumberOfTypeArguments {
        FType type = getType(ctx.typeType(), possibleTypes);
        FIdentifier identifier = getVarIdentifier(ctx.identifier());
        boolean hasDefaultValue = ctx.expression() != null;
        return FParameter.create(identifier, type, hasDefaultValue);
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

    public static FIdentifier getVarIdentifier(FrontierParser.IdentifierContext ctx) {
        String text = ctx.getText();
        char start = text.charAt(0);
        if (Character.isUpperCase(start))
            return new FTypeIdentifier(text);
        else if (Character.isLowerCase(start))
            return new FVariableIdentifier(text);
        else
            return Utils.cantHappen();
    }
}

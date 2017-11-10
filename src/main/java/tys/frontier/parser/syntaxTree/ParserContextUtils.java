package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.FClass;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.FBoolLiteral;
import tys.frontier.code.literal.FInt32Literal;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.ClassNotFound;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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

    public static FClass getNonPredefined(String id, Map<FClassIdentifier, FClass> possibleTypes) throws ClassNotFound {
        FClass type;
        FClassIdentifier identifier = new FClassIdentifier(id);
        type = possibleTypes.get(identifier);
        if (type==null) {
            throw new ClassNotFound(identifier);
        }
        return type;
    }

    public static FClass getBasicType (FrontierParser.BasicTypeContext ctx, Map<FClassIdentifier, FClass> possibleTypes)
            throws ClassNotFound {
        FrontierParser.PredefinedTypeContext predefined = ctx.predefinedType();
        return predefined != null ? getPredefined(predefined) : getNonPredefined(ctx.TypeIdentifier().getText(), possibleTypes);
    }

    public static FClass getType (FrontierParser.TypeTypeContext ctx, Map<FClassIdentifier, FClass> possibleTypes)
            throws ClassNotFound{
        FClass res = getBasicType(ctx.basicType(), possibleTypes);
        int arrayDepth = ctx.Array().size();
        if (arrayDepth > 0)
            res = FArray.getArrayFrom(res, arrayDepth);
        return res;
    }

    public static FLocalVariable getVariable (FrontierParser.TypedIdentifierContext ctx, Map<FClassIdentifier, FClass> possibleTypes)
            throws ClassNotFound{
        FClass type = getType(ctx.typeType(), possibleTypes);
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        return new FLocalVariable(identifier, type);
    }

    public static FLiteral getLiteral (FrontierParser.LiteralContext ctx) {
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
                        throw new RuntimeException("no longs for now");
                    res = new FInt32Literal(Integer.parseInt(text), text);
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

    public static ImmutableList<FLocalVariable> getParams (FrontierParser.FormalParametersContext ctx, Map<FClassIdentifier, FClass> possibleTypes)
            throws SyntaxErrors {
        List<FrontierParser.TypedIdentifierContext> cs = ctx.typedIdentifier();
        if (cs.isEmpty())
            return ImmutableList.of();
        ImmutableList.Builder<FLocalVariable> res = ImmutableList.builder();
        List<ClassNotFound> errors = new ArrayList<>();
        for (FrontierParser.TypedIdentifierContext c : cs) {
            try {
                res.add(getVariable(c, possibleTypes));
            } catch (ClassNotFound e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty())
            throw new SyntaxErrors(errors);
        return res.build();
    }


}

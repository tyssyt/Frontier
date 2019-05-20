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
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;
import java.util.function.Function;

import static tys.frontier.parser.antlr.FrontierParser.*;

public final class ParserContextUtils {

    private ParserContextUtils() {}

    public static FClass getClass (FrontierParser.ClassDeclarationContext ctx) throws TwiceDefinedLocalVariable {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        FTypeIdentifier identifier = new FTypeIdentifier(ctx.TypeIdentifier().getText());
        FrontierParser.TypeParametersContext c = ctx.typeParameters();
        FClass res =  new FClass(identifier, visibilityModifier);
        if (c != null) {
            Pair<List<FTypeVariable>, List<Variance>> typeParameters = getTypeParameters(c);
            res.setParameters(typeParameters.a, typeParameters.b);
        }
        return res;
    }

    public static Pair<List<FTypeVariable>, List<Variance>> getTypeParameters(FrontierParser.TypeParametersContext ctx) throws TwiceDefinedLocalVariable {
        List<TypeParamerContext> nodes = ctx.typeParamer();
        List<FTypeVariable> vars = new ArrayList<>(nodes.size());
        List<Variance> variances = new ArrayList<>(nodes.size());
        Set<FTypeIdentifier> seem = new HashSet<>();
        for (TypeParamerContext c : nodes) {
            FTypeIdentifier id = new FTypeIdentifier(c.TypeIdentifier().getText());
            if (!seem.add(id))
                throw new TwiceDefinedLocalVariable(id);

            boolean fixed = c.STAR() == null;
            vars.add(FTypeVariable.create(id, fixed));
            Variance variance;
            if(c.IN() != null)
                variance = Variance.Contravariant;
            else if (c.OUT() != null)
                variance = Variance.Covariant;
            else
                variance = Variance.Invariant;
            variances.add(variance);
        }
        return new Pair<>(vars, variances);
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

    public static void handleTypeParameterSpecification(TypeParameterSpecificationContext ctx, Map<FTypeIdentifier, FTypeVariable> params, Function<FTypeIdentifier, FType> possibleTypes) throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable, UnfulfillableConstraints, UndeclaredVariable {
        FTypeIdentifier identifier = new FTypeIdentifier(ctx.TypeIdentifier().getText());
        FTypeVariable typeVariable = params.get(identifier);
        if (typeVariable == null)
            throw new UndeclaredVariable(identifier);

        TypeConstraints constraints = TypeConstraints.create();
        UpperBoundContext uC = ctx.upperBound();
        if (uC != null) {
            for (FType type : typesFromList(uC.typeList().typeType(), possibleTypes))
                constraints = TypeConstraints.add(constraints, new ImplicitCastable(ctx, type, Variance.Contravariant));
        }
        LowerBoundContext lC = ctx.lowerBound();
        if (lC != null) {
            for (FType type : typesFromList(lC.typeList().typeType(), possibleTypes))
                constraints = TypeConstraints.add(constraints, new ImplicitCastable(ctx, type, Variance.Covariant));
        }
        typeVariable.setConstraints(constraints);
    }

    public static FPredefinedClass getPredefined (FrontierParser.PredefinedTypeContext ctx) {
        switch (((TerminalNode)ctx.children.get(0)).getSymbol().getType()) {
            case BOOL:
                return FBool.INSTANCE;
            case INT:
                return Utils.NYI("unbounded int type");
            case CHAR:
                return FIntN._8;
            case INT32:
                return FIntN._32;
            case INT64:
                return FIntN._64;
            case FLOAT32:
                return FFloat32.INSTANCE;
            case FLOAT64:
                return FFloat64.INSTANCE;
            case TYPE:
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

    public static List<FType> typesFromList(List<TypeTypeContext> contexts, Function<FTypeIdentifier, FType> possibleTypes)
            throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable {
        List<FType> res = new ArrayList<>(contexts.size());
        for (TypeTypeContext c : contexts) {
            res.add(getType(c, possibleTypes));
        }
        return res;
    }

    public static FType getType (FrontierParser.TypeTypeContext ctx, Function<FTypeIdentifier, FType> possibleTypes)
            throws TypeNotFound, ParameterizedTypeVariable, WrongNumberOfTypeArguments {
        FType base;
        if (ctx.Array() != null) {
            base = getType(ctx.typeType(0), possibleTypes);
            return FArray.getArrayFrom(base);
        } else if (ctx.QUESTION() != null) {
            base = getType(ctx.typeType(0), possibleTypes);
            return FOptional.from(base);
        } else if (ctx.ARROW() != null) {
            List<FType> types = typesFromList(ctx.typeType(), possibleTypes);
            return FFunctionType.from(types.subList(0, types.size()-1), types.get(types.size()-1));
        } else if (ctx.predefinedType() != null) {
            base = getPredefined(ctx.predefinedType());
        } else if (ctx.TypeIdentifier() != null) {
            base = getNonPredefined(ctx.TypeIdentifier().getText(), possibleTypes);
        } else if (ctx.typeType(0) != null) {
            return getType(ctx.typeType(0), possibleTypes);
        } else {
            return FVoid.INSTANCE;
        }

        //handle Type Parameters
        List<FType> parameters;
        if (ctx.typeList() == null)
            parameters = Collections.emptyList();
        else
            parameters = typesFromList(ctx.typeList().typeType(), possibleTypes);
        if (base instanceof FClass)
            return ((FClass) base).getInstantiation(parameters);
        else if (parameters.size() != 0)
            throw new ParameterizedTypeVariable(null); //TODO
        else
            return base;
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
                    res = new FFloat64Literal(Double.parseDouble(text), text);
                    break;
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
                    String string = sb.toString();
                    if (string.length() == 1) {
                        return new FCharLiteral(string.charAt(0));
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

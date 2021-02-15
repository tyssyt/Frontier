package tys.frontier.parser.syntaxTree;

import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.function.NativeDecl;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.*;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.selector.Selector;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.math.BigInteger;
import java.util.*;
import java.util.function.Function;

import static tys.frontier.parser.antlr.FrontierParser.*;

public final class ParserContextUtils {

    private ParserContextUtils() {}

    public static Pair<List<FTypeVariable>, List<Variance>> getTypeParameters(FrontierParser.TypeParametersContext ctx) throws TwiceDefinedLocalVariable {
        List<TypeParamerContext> nodes = ctx.typeParamer();
        List<FTypeVariable> vars = new ArrayList<>(nodes.size());
        List<Variance> variances = new ArrayList<>(nodes.size());
        Map<FIdentifier, TypeParamerContext> seen = new HashMap<>();
        for (TypeParamerContext c : nodes) {
            FIdentifier id = new FIdentifier(c.IDENTIFIER().getText());
            TypeParamerContext old = seen.put(id, c);
            if (old != null)
                throw new TwiceDefinedLocalVariable(Position.fromCtx(c), Position.fromCtx(old), id);

            boolean fixed = c.STAR() == null;
            vars.add(FTypeVariable.create(null, id, fixed)); //TODO get file from somewhere
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
        return switch (((TerminalNode) ctx.children.get(0)).getSymbol().getType()) {
            case FrontierParser.EXPORT  -> FVisibilityModifier.EXPORT;
            case FrontierParser.PRIVATE -> FVisibilityModifier.PRIVATE;
            default -> null;
        };
    }

    public static boolean isStatic (FrontierParser.ModifierContext ctx) {
        return ctx !=  null;
    }

    public static NativeDecl getNative (FrontierParser.NativeModifierContext ctx) {
        if (ctx == null)
            return null;
        TerminalNode stringLiteral = ctx.StringLiteral();
        if (stringLiteral == null)
            return new NativeDecl(null);
        else
            return new NativeDecl(getStringLiteral(stringLiteral.getSymbol()));
    }

    public static void handleTypeParameterSpecification(TypeParameterSpecificationContext ctx, Map<FIdentifier, FTypeVariable> params, Function<FIdentifier, Namespace> possibleNamespaces) throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable, UnfulfillableConstraints, UndeclaredVariable, NonEmbeddableType {
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        FTypeVariable typeVariable = params.get(identifier);
        if (typeVariable == null)
            throw new UndeclaredVariable(Position.fromCtx(ctx), identifier);

        TypeConstraints constraints = TypeConstraints.create();
        constraints.getEquivalenceGroup().add(typeVariable);
        UpperBoundContext uC = ctx.upperBound();
        if (uC != null) {
            for (FType type : typeListFromList(uC.typeList(), possibleNamespaces))
                constraints = TypeConstraints.add(constraints, new ImplicitCastable(ctx, type, Variance.Contravariant));
        }
        LowerBoundContext lC = ctx.lowerBound();
        if (lC != null) {
            for (FType type : typeListFromList(lC.typeList(), possibleNamespaces))
                constraints = TypeConstraints.add(constraints, new ImplicitCastable(ctx, type, Variance.Covariant));
        }
        if (typeVariable.isFixed())
            constraints.setFixed();
        typeVariable.setConstraints(constraints);
    }

    public static FBaseClass getPredefined (FrontierParser.PredefinedTypeContext ctx) {
        return switch (((TerminalNode) ctx.children.get(0)).getSymbol().getType()) {
            case BOOL    -> FBool.INSTANCE;
            case INT     -> Utils.NYI("unbounded int type");
            case CHAR    -> FIntN._8;
            case INT16   -> FIntN._16;
            case INT32   -> FIntN._32;
            case INT64   -> FIntN._64;
            case FLOAT32 -> FFloat32.INSTANCE;
            case FLOAT64 -> FFloat64.INSTANCE;
            default      -> Utils.NYI("Frontier type for: " + ((TerminalNode) ctx.children.get(0)).getSymbol().getText());
        };
    }

    public static FType getUserType(FrontierParser.UserTypeContext ctx, Function<FIdentifier, Namespace> possibleNamespaces)
            throws TypeNotFound, ParameterizedTypeVariable, WrongNumberOfTypeArguments, NonEmbeddableType {
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        Namespace namespace = possibleNamespaces.apply(identifier);
        if (namespace==null)
            throw new TypeNotFound(Position.fromCtx(ctx), identifier);
        FType base = namespace.getType();
        if (base == null)
            throw new NamespaceWithoutType(Position.fromCtx(ctx), namespace);

        //handle Type Parameters
        List<FType> parameters = new ArrayList<>();
        for (TypeOrTupleContext ttc : ctx.typeOrTuple())
            parameters.add(getType(ttc, possibleNamespaces));

        if (base instanceof FClass)
            return ((FClass) base).getInstantiation(parameters);
        else if (parameters.size() != 0)
            throw new ParameterizedTypeVariable((FTypeVariable) base);
        else
            return base;
    }

    public static FType tupleFromList(TypeListContext ctx, Function<FIdentifier, Namespace> possibleNamespaces)
            throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable, NonEmbeddableType {
        return FTuple.from(typeListFromList(ctx, possibleNamespaces));
    }

    public static List<FType> typeListFromList(TypeListContext ctx, Function<FIdentifier, Namespace> possibleNamespaces)
            throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable, NonEmbeddableType {
        List<TypeTypeContext> cs = ctx.typeType();
        List<FType> res = new ArrayList<>(cs.size());
        for (TypeTypeContext c : cs) {
            res.add(getType(c, possibleNamespaces));
        }
        return res;
    }

    public static Namespace getNamespace (TypeTypeContext ctx, Function<FIdentifier, Namespace> possibleNamespaces)
            throws TypeNotFound, ParameterizedTypeVariable, WrongNumberOfTypeArguments, NonEmbeddableType {
        try {
            return getType(ctx, possibleNamespaces).getNamespace();
        } catch (NamespaceWithoutType e) {
            return e.namespace;
        }
    }

    public static FType getType (TypeOrTupleContext ctx, Function<FIdentifier, Namespace> possibleNamespaces) throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable, NonEmbeddableType {
        TypeTypeContext c = ctx.typeType();
        if (c != null)
            return getType(c, possibleNamespaces);
        else
            return tupleFromList(ctx.typeList(), possibleNamespaces);
    }

    public static FType getType (TypeTypeContext ctx, Function<FIdentifier, Namespace> possibleNamespaces)
            throws TypeNotFound, ParameterizedTypeVariable, WrongNumberOfTypeArguments, NonEmbeddableType {
        if (ctx.NATIVE() != null)
            return CArray.getArrayFrom(getType(ctx.typeOrTuple(), possibleNamespaces));
        else if (ctx.LBRACK() != null)
            return FArray.getArrayFrom(getType(ctx.typeOrTuple(), possibleNamespaces));
        else if (ctx.QUESTION() != null)
            return FOptional.from(getType(ctx.typeType(), possibleNamespaces));
        else if (ctx.ARROW() != null) {
            List<TypeListContext> typeListContexts = ctx.typeList();
            FType  in = tupleFromList(typeListContexts.get(0), possibleNamespaces);
            FType out = tupleFromList(typeListContexts.get(1), possibleNamespaces);
            return FFunctionType.from(in, out);
        } else if (ctx.predefinedType() != null)
            return getPredefined(ctx.predefinedType());
        else if (ctx.userType() != null)
            return getUserType(ctx.userType(), possibleNamespaces);
        else
            return Utils.cantHappen();
    }

    public static Selector<FIdentifier> getNameSelector(FrontierParser.NameSelectorContext ctx) {
        if (ctx.STAR() != null && ctx.BACKSLASH() == null)
            return Selector.all();
        List<TerminalNode> nodes = ctx.IDENTIFIER();
        List<FIdentifier> res = new ArrayList<>(nodes.size());
        for (TerminalNode node : nodes) {
            res.add(new FIdentifier(node.getText()));
        }
        if (ctx.BACKSLASH() == null)
            return Selector.in(res);
        else
            return Selector.notIn(res);
    }

    public static FParameter getParameter (FrontierParser.FormalParameterContext ctx, Function<FIdentifier, Namespace> possibleNamespaces)
            throws TypeNotFound, ParameterizedTypeVariable, WrongNumberOfTypeArguments, NonEmbeddableType {
        boolean hasDefaultValue = ctx.expression() != null;
        Pair<FIdentifier, FType> pair = getTypedIdentifier(ctx.typedIdentifier(), possibleNamespaces);
        if (!hasDefaultValue && FOptional.canBeTreatedAsOptional(pair.b)) {
            FParameter res = FParameter.create(Position.fromCtx(ctx), pair.a, pair.b, true);
            //TODO @PositionForGeneratedCode
            res.setDefaultValueTrusted(new FLiteralExpression(null, new FNull(pair.b)), Set.of());
            return res;
        }
        return FParameter.create(Position.fromCtx(ctx), pair.a, pair.b, hasDefaultValue);
    }

    public static Pair<FIdentifier, FType> getTypedIdentifier (FrontierParser.TypedIdentifierContext ctx, Function<FIdentifier, Namespace> possibleNamespaces)
            throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable, NonEmbeddableType {
        FType type = getType(ctx.typeType(), possibleNamespaces);
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        return new Pair<>(identifier, type);
    }

    public static FLiteral getLiteral (FrontierParser.LiteralContext ctx) { //TODO why do we have res instead of just return (look at once all literals are done)
        ParseTree child = ctx.getChild(0);
        if (child instanceof FrontierParser.BooleanLiteralContext) {
            if (((FrontierParser.BooleanLiteralContext) child).TRUE() != null)
                return FBoolLiteral.TRUE;
            else
                return FBoolLiteral.FALSE;
        }
        assert child instanceof TerminalNode;
        Token token = ((TerminalNode) child).getSymbol();
        String text = token.getText();
        return switch (token.getType()) {
            case IntegerLiteral       -> new FIntNLiteral(new BigInteger(text), text);
            case NULL                 -> FNull.UNTYPED;
            case FloatingPointLiteral ->
                    switch (text.charAt(text.length() - 1)) {
                        case 'f', 'F' -> new FFloat32Literal(Float.parseFloat(text), text);
                        default       -> new FFloat64Literal(Double.parseDouble(text), text);
                    };
            case StringLiteral        -> {
                String string = getStringLiteral(token);
                yield string.length() == 1 ? new FCharLiteral(string.charAt(0)) : new FStringLiteral(string);
            }
            default                   -> Utils.cantHappen();
        };
    }

    public static String getStringLiteral(Token token) {
        String text = token.getText();
        assert text.charAt(0) == '\"';
        assert text.charAt(text.length() - 1) == '\"';
        StringBuilder sb = new StringBuilder();
        for (int i = 1; i < text.length() - 1; i++) {
            char c = text.charAt(i);
            if (c == '\\') {
                i++;
                c = FCharLiteral.escapeLiterals.get(text.charAt(i));
                assert c != 0 || text.charAt(i) == '0';
            }
            sb.append(c);
        }
        return sb.toString();
    }

    public static Set<FParameter> findDefaultValueDependencies(FExpression defaultValue, List<FParameter> parameters) {
        Set<FParameter> defaultValueDependencies = new HashSet<>();
        defaultValue.accept(new ExpressionVisitor<>() {
            @Override
            @SuppressWarnings("SuspiciousMethodCalls")
            public Object visitVariable(FVariableExpression expression) {
                if (parameters.contains(expression.getVariable()))
                    defaultValueDependencies.add((FParameter) expression.getVariable());
                return null;
            }
        });
        return defaultValueDependencies;
    }
}

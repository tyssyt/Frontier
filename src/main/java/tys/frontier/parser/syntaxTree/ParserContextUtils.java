package tys.frontier.parser.syntaxTree;

import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.*;
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
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.math.BigInteger;
import java.util.*;
import java.util.function.Function;

import static tys.frontier.parser.antlr.FrontierParser.*;

public final class ParserContextUtils {

    private ParserContextUtils() {}

    public static FClass getClass (FrontierParser.ClassDeclarationContext ctx) throws TwiceDefinedLocalVariable {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        boolean _native = ctx.NATIVE() != null;
        FrontierParser.TypeParametersContext c = ctx.typeParameters();
        FClass res =  new FBaseClass(identifier, visibilityModifier, _native);
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
        Set<FIdentifier> seem = new HashSet<>();
        for (TypeParamerContext c : nodes) {
            FIdentifier id = new FIdentifier(c.IDENTIFIER().getText());
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

    public static void handleTypeParameterSpecification(TypeParameterSpecificationContext ctx, Map<FIdentifier, FTypeVariable> params, Function<FIdentifier, FType> possibleTypes) throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable, UnfulfillableConstraints, UndeclaredVariable {
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        FTypeVariable typeVariable = params.get(identifier);
        if (typeVariable == null)
            throw new UndeclaredVariable(identifier);

        TypeConstraints constraints = TypeConstraints.create();
        UpperBoundContext uC = ctx.upperBound();
        if (uC != null) {
            for (FType type : typeListFromList(uC.typeList(), possibleTypes))
                constraints = TypeConstraints.add(constraints, new ImplicitCastable(ctx, type, Variance.Contravariant));
        }
        LowerBoundContext lC = ctx.lowerBound();
        if (lC != null) {
            for (FType type : typeListFromList(lC.typeList(), possibleTypes))
                constraints = TypeConstraints.add(constraints, new ImplicitCastable(ctx, type, Variance.Covariant));
        }
        typeVariable.setConstraints(constraints);
    }

    public static FBaseClass getPredefined (FrontierParser.PredefinedTypeContext ctx) {
        switch (((TerminalNode)ctx.children.get(0)).getSymbol().getType()) {
            case BOOL:
                return FBool.INSTANCE;
            case INT:
                return Utils.NYI("unbounded int type");
            case CHAR:
                return FIntN._8;
            case INT16:
                return FIntN._16;
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

    public static FType getNonPredefined(String id, Function<FIdentifier, FType> possibleTypes) throws TypeNotFound {
        FIdentifier identifier = new FIdentifier(id);
        FType type = possibleTypes.apply(identifier);
        if (type==null) {
            throw new TypeNotFound(identifier);
        }
        return type;
    }

    public static FType tupleFromList(TypeListContext ctx, Function<FIdentifier, FType> possibleTypes)
            throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable {
        return FTuple.from(typeListFromList(ctx, possibleTypes));
    }

    public static List<FType> typeListFromList(TypeListContext ctx, Function<FIdentifier, FType> possibleTypes)
            throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable {
        List<TypeTypeContext> cs = ctx.typeType();
        List<FType> res = new ArrayList<>(cs.size());
        for (TypeTypeContext c : cs) {
            res.add(getType(c, possibleTypes));
        }
        return res;
    }

    public static FType getType (FrontierParser.TypeTypeContext ctx, Function<FIdentifier, FType> possibleTypes)
            throws TypeNotFound, ParameterizedTypeVariable, WrongNumberOfTypeArguments {
        FType base;
        if (ctx.Array() != null) {
            base = getType(ctx.typeType(), possibleTypes);
            return FArray.getArrayFrom(base);
        } if (ctx.CArray() != null) {
            base = getType(ctx.typeType(), possibleTypes);
            return tys.frontier.code.predefinedClasses.CArray.getArrayFrom(base);
        } else if (ctx.QUESTION() != null) {
            base = getType(ctx.typeType(), possibleTypes);
            return FOptional.from(base);
        } else if (ctx.ARROW() != null) {
            List<TypeListContext> typeListContexts = ctx.typeList();
            FType  in = tupleFromList(typeListContexts.get(0), possibleTypes);
            FType out = tupleFromList(typeListContexts.get(1), possibleTypes);
            return FFunctionType.from(in, out);
        } else if (ctx.predefinedType() != null) {
            base = getPredefined(ctx.predefinedType());
        } else if (ctx.IDENTIFIER() != null) {
            base = getNonPredefined(ctx.IDENTIFIER().getText(), possibleTypes);
        } else if (ctx.typeType() != null) {
            return getType(ctx.typeType(), possibleTypes);
        } else {
            return Utils.cantHappen();
        }

        //handle Type Parameters
        List<TypeOrTupleContext> ttCtxs = ctx.typeOrTuple();

        List<FType> parameters = new ArrayList<>();
        for (TypeOrTupleContext ttc : ttCtxs) {
            TypeTypeContext c = ttc.typeType();
            if (c != null)
                parameters.add(getType(c, possibleTypes));
            else
                parameters.add(tupleFromList(ttc.typeList(), possibleTypes));
        }

        if (base instanceof FClass)
            return ((FClass) base).getInstantiation(parameters);
        else if (parameters.size() != 0)
            throw new ParameterizedTypeVariable(null); //TODO
        else
            return base;
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

    public static FParameter getParameter (FrontierParser.FormalParameterContext ctx, Function<FIdentifier, FType> possibleTypes)
            throws TypeNotFound, ParameterizedTypeVariable, WrongNumberOfTypeArguments {
        boolean hasDefaultValue = ctx.expression() != null;
        Pair<FIdentifier, FType> pair = getTypedIdentifier(ctx.typedIdentifier(), possibleTypes);
        return FParameter.create(pair.a, pair.b, hasDefaultValue);
    }

    public static Pair<FIdentifier, FType> getTypedIdentifier (FrontierParser.TypedIdentifierContext ctx, Function<FIdentifier, FType> possibleTypes)
            throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable {
        FType type = getType(ctx.typeType(), possibleTypes);
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        return new Pair<>(identifier, type);
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
                    res = new FIntNLiteral(new BigInteger(text), text);
                    break;
                case FrontierParser.NULL:
                    res = FNull.UNTYPED;
                    break;
                case FrontierParser.FloatingPointLiteral:
                    switch (text.charAt(text.length()-1)) {
                        case 'f': case 'F':
                            res = new FFloat32Literal(Float.parseFloat(text), text);
                            break;
                        case 'd': case 'D': default:
                            res = new FFloat64Literal(Double.parseDouble(text), text);
                            break;
                    }
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
                            assert c != 0 || text.charAt(i) == '0';
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

    public static Set<FParameter> findDefaultValueDependencies(FExpression defaultValue, List<FParameter> parameters) {
        Set<FParameter> defaultValueDependencies = new HashSet<>();
        defaultValue.accept(new ExpressionVisitor<Object>() {
            @Override
            @SuppressWarnings("SuspiciousMethodCalls")
            public Object visitVariable(FLocalVariableExpression expression) {
                if (parameters.contains(expression.getVariable()))
                    defaultValueDependencies.add((FParameter) expression.getVariable());
                return null;
            }
        });
        return defaultValueDependencies;
    }
}

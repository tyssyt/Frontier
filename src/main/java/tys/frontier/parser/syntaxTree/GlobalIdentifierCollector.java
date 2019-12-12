package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.operator.Operator;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.Delegates;
import tys.frontier.parser.ParsedFile;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.parser.syntaxErrors.TwiceDefinedLocalVariable;
import tys.frontier.util.Utils;

import java.util.*;
import java.util.function.Function;

public class GlobalIdentifierCollector extends FrontierBaseVisitor<Object> {

    private ParsedFile file;
    private SyntaxTreeData treeData;
    private Delegates delegates;
    private List<SyntaxError> errors;

    private FClass currentClass;

    private GlobalIdentifierCollector(ParsedFile file, Delegates delegates, List<SyntaxError> errors) {
        this.file = file;
        this.treeData = file.getTreeData();
        this.delegates = delegates;
        this.errors = errors;
    }

    public static Delegates collectIdentifiers(ParsedFile file, Delegates delegates, List<SyntaxError> errors) {
        GlobalIdentifierCollector collector = new GlobalIdentifierCollector(file, delegates, errors);
        for (FrontierParser.ClassDeclarationContext ctx : collector.treeData.root.classDeclaration()) {
            ctx.accept(collector);
        }
        return collector.delegates;
    }

    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentClass = treeData.classes.get(ctx);
        try {
            visitChildren(ctx);
            return null;
        } finally {
            currentClass = null;
        }
    }

    @Override
    public Object visitConstructorsDeclarative(FrontierParser.ConstructorsDeclarativeContext ctx) {
        currentClass.setConstructorVisibility(ParserContextUtils.getVisibility(ctx.visibilityModifier()));
        return null;
    }

    @Override
    public Object visitMethodHeader(FrontierParser.MethodHeaderContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        boolean natiwe = ctx.NATIVE() != null;

        //type Parameters
        Map<FTypeIdentifier, FTypeVariable> typeParameters;
        Function<FTypeIdentifier, FType> typeResolver;
        {
            FrontierParser.TypeParametersContext c = ctx.typeParameters();
            if (c != null) {
                try {
                    typeParameters = Utils.asTypeMap(ParserContextUtils.getTypeParameters(c).a);
                    for (FType classParam : currentClass.getParametersList()) {
                        if (typeParameters.containsKey(classParam.getIdentifier())) {
                            throw new TwiceDefinedLocalVariable(classParam.getIdentifier());
                        }
                    }
                    typeResolver = id -> Optional.<FType>ofNullable(typeParameters.get(id)).orElseGet(() -> resolveType(id));
                } catch (TwiceDefinedLocalVariable twiceDefinedLocalVariable) {
                    errors.add(twiceDefinedLocalVariable);
                    return null;
                }
            } else {
                typeParameters = Collections.emptyMap();
                typeResolver = this::resolveType;
            }
        }

        //return type
        FrontierParser.TypeListContext c = ctx.typeList();
        FType returnType;
        if (c != null) {
            try {
                returnType = ParserContextUtils.tupleFromList(c, typeResolver);
            } catch (SyntaxError e) {
                errors.add(e);
                returnType = FTuple.VOID; //TODO do we want some error related type here?
            }
        } else {
            returnType = FTuple.VOID;
        }

        ImmutableList.Builder<FParameter> params = ImmutableList.builder();
        if (ctx.STATIC() == null) {
            params.add(FParameter.create(FVariableIdentifier.THIS, currentClass, false));
        }


        try {
            formalParameters(ctx.formalParameters(), params, typeResolver);
            ImmutableList<FParameter> parameters = params.build();

            //identifier
            FFunctionIdentifier identifier;
            TerminalNode identifierNode = ctx.LCIdentifier();
            if (identifierNode != null) {
                identifier = new FFunctionIdentifier(identifierNode.getText());
            } else {
                //Operator overloading
                Operator operator = Operator.get(ctx.operator().getText(), Utils.typesFromExpressionList(parameters));
                if (!operator.isUserDefinable())
                    return Utils.NYI("non overridable Operator aka FunctionNotFoundOrSth"); //TODO
                identifier = operator.getIdentifier();
            }

            FFunction res = new FBaseFunction(identifier, currentClass, visibilityModifier, natiwe, returnType, parameters, typeParameters);
            currentClass.addFunction(res);
            treeData.functions.put(ctx, res);
        } catch (SyntaxErrors e) {
            errors.addAll(e.errors);
        } catch (SignatureCollision e) {
            errors.add(e);
        }
        return null;
    }

    public void formalParameters(FrontierParser.FormalParametersContext ctx, ImmutableList.Builder<FParameter> params, Function<FTypeIdentifier, FType> possibleTypes) throws SyntaxErrors {
        List<FrontierParser.FormalParameterContext> cs = ctx.formalParameter();
        List<SyntaxError> errors = new ArrayList<>();
        for (FrontierParser.FormalParameterContext c : cs) {
            try {
                FParameter param = ParserContextUtils.getParameter(c, possibleTypes);
                treeData.parameters.put(c, param);
                params.add(param);
            } catch (SyntaxError e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }

    @Override
    public Object visitFieldDeclaration(FrontierParser.FieldDeclarationContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        boolean statik = ParserContextUtils.isStatic(ctx.modifier());
        try {
            FIdentifier identifier = ParserContextUtils.getVarIdentifier(ctx.identifier());
            FType type = ParserContextUtils.getType(ctx.typeType(), this::resolveType);
            FField res = new FField(identifier, type, currentClass, visibilityModifier, statik, ctx.expression() != null);
            currentClass.addField(res);
            treeData.fields.put(ctx, res);

            FrontierParser.NameSelectorContext c = ctx.nameSelector();
            if (c != null) {
                if (c.STAR() != null && c.BACKSLASH() == null)
                    currentClass.addDelegate(res);
                delegates.add(res, ParserContextUtils.getNameSelector(c));
            }

        } catch (SyntaxError e) {
            errors.add(e);
        }
        return null;
    }

    private FType resolveType(FTypeIdentifier identifier) {
        for (FType p : currentClass.getParametersList()) {
            if (p.getIdentifier().equals(identifier))
                return p;
        }
        return file.resolveType(identifier);
    }
}

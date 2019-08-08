package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.Delegates;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;
import java.util.function.Function;

public class GlobalIdentifierCollector extends FrontierBaseVisitor {

    private Map<FTypeIdentifier, FType> types;
    private SyntaxTreeData treeData;
    private Delegates delegates = new Delegates();
    private List<SyntaxError> errors = new ArrayList<>();

    private FClass currentClass;

    private GlobalIdentifierCollector (FrontierParser.FileContext ctx, Module module) {
        treeData = new SyntaxTreeData(ctx);
        types = new HashMap<>(module.getImportedClasses());
        ctx.accept(this);
    }

    public static Pair<SyntaxTreeData, Delegates> getIdentifiers(FrontierParser.FileContext ctx, Module module) throws SyntaxErrors {
        GlobalIdentifierCollector collector = new GlobalIdentifierCollector(ctx, module);
        if (!collector.errors.isEmpty())
            throw SyntaxErrors.create(collector.errors);
        return new Pair<>(collector.treeData, collector.delegates);
    }


    @Override
    public Object visitFile(FrontierParser.FileContext ctx) {
        //first go over all types once to know their names
        for (FrontierParser.ClassDeclarationContext c : ctx.classDeclaration()) {
            try {
                FClass fClass = ParserContextUtils.getClass(c);
                FType old = types.put(fClass.getIdentifier(), fClass);
                if (old != null) {
                    errors.add(new IdentifierCollision(fClass, old));
                }
                treeData.classes.put(c, fClass);
            } catch (TwiceDefinedLocalVariable twiceDefinedLocalVariable) {
                errors.add(twiceDefinedLocalVariable);
            }
        }
        if (!errors.isEmpty())
            return null;
        //in the second pass find fields and methods headers
        return visitChildren(ctx);
    }

    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentClass = treeData.classes.get(ctx);
        for (FType p : currentClass.getParametersList()) {
            FType old = types.put(p.getIdentifier(), p);
            if (old != null)
                errors.add(new IdentifierCollision(old, p));
        }
        try {
            visitChildren(ctx);
            return null;
        } finally {
            for (FType p : currentClass.getParametersList()) {
                types.remove(p.getIdentifier());
            }
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
                    typeResolver = id -> {
                        FType t = types.get(id);
                        return t == null ? typeParameters.get(id) : t;
                    };
                } catch (TwiceDefinedLocalVariable twiceDefinedLocalVariable) {
                    errors.add(twiceDefinedLocalVariable);
                    return null;
                }
            } else {
                typeParameters = Collections.emptyMap();
                typeResolver = types::get;
            }
        }

        //return type
        FrontierParser.TypeTypeContext c = ctx.typeType();
        FType returnType;
        if (c != null) {
            try {
                returnType = ParserContextUtils.getType(c, typeResolver);
            } catch (SyntaxError e) {
                errors.add(e);
                returnType = FVoid.INSTANCE; //TODO do we want some error related type here?
            }
        } else {
            returnType = FVoid.INSTANCE;
        }

        ImmutableList.Builder<FParameter> params = ImmutableList.builder();
        if (ctx.STATIC() == null) {
            params.add(FParameter.create(FVariableIdentifier.THIS, currentClass, false));
        }

        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.LCIdentifier().getText());
        try {
            formalParameters(ctx.formalParameters(), params, typeResolver);
            FFunction res = new FBaseFunction(identifier, currentClass, visibilityModifier, natiwe, returnType, params.build(), typeParameters);
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
            FType type = ParserContextUtils.getType(ctx.typeType(), types::get);
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
}

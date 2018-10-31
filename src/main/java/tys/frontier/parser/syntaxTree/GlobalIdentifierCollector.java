package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.parser.Delegates;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GlobalIdentifierCollector extends FrontierBaseVisitor {

    private Map<FTypeIdentifier, FType> types;
    private SyntaxTreeData treeData;
    private Delegates delegates = new Delegates();
    private List<SyntaxError> errors = new ArrayList<>();

    private FClass currentClass;

    private GlobalIdentifierCollector (FrontierParser.FileContext ctx) {
        treeData = new SyntaxTreeData(ctx);
        ctx.accept(this);
    }

    public static Pair<SyntaxTreeData, Delegates> getIdentifiers(FrontierParser.FileContext ctx) throws SyntaxErrors {
        GlobalIdentifierCollector collector = new GlobalIdentifierCollector(ctx);
        if (!collector.errors.isEmpty())
            throw SyntaxErrors.create(collector.errors);
        return new Pair<>(collector.treeData, collector.delegates);
    }


    @Override
    public Object visitFile(FrontierParser.FileContext ctx) {
        types = new HashMap<>(ctx.children.size());
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
        for (Map.Entry<FTypeIdentifier, FTypeVariable> entry : currentClass.getParameters().entrySet()) {
            FType old = types.put(entry.getKey(), entry.getValue());
            if (old != null)
                errors.add(new IdentifierCollision(old, entry.getValue()));
        }
        try {
            visitChildren(ctx);
            return null;
        } finally {
            for (FTypeIdentifier identifier : currentClass.getParameters().keySet()) {
                types.remove(identifier);
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
        boolean statik = ctx.STATIC() != null;
        //return type
        FrontierParser.TypeTypeContext c = ctx.typeType();
        FType returnType;
        if (c != null) {
            try {
                returnType = ParserContextUtils.getType(c, types);
            } catch (SyntaxError e) {
                errors.add(e);
                returnType = FVoid.INSTANCE; //TODO do we want some error related type here?
            }
        } else {
            returnType = FVoid.INSTANCE;
        }

        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.LCIdentifier().getText());
        try {
            ImmutableList<FParameter> params = formalParameters(ctx.formalParameters());
            FFunction res = new FFunction(identifier, currentClass, visibilityModifier, natiwe, statik, returnType, params);
            currentClass.addFunction(res);
            treeData.functions.put(ctx, res);
        } catch (SyntaxErrors e) {
            errors.addAll(e.errors);
        } catch (SignatureCollision e) {
            errors.add(e);
        }
        return null;
    }

    public ImmutableList<FParameter> formalParameters(FrontierParser.FormalParametersContext ctx) throws SyntaxErrors {
        List<FrontierParser.FormalParameterContext> cs = ctx.formalParameter();
        if (cs.isEmpty())
            return ImmutableList.of();
        ImmutableList.Builder<FParameter> res = ImmutableList.builder();
        List<SyntaxError> errors = new ArrayList<>();
        for (FrontierParser.FormalParameterContext c : cs) {
            try {
                FParameter param = ParserContextUtils.getParameter(c, types);
                treeData.parameters.put(c, param);
                res.add(param);
            } catch (SyntaxError e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
        return res.build();
    }

    @Override
    public Object visitFieldDeclaration(FrontierParser.FieldDeclarationContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        boolean statik = ParserContextUtils.isStatic(ctx.modifier());
        try {
            FIdentifier identifier = ParserContextUtils.getVarIdentifier(ctx.identifier());
            FType type = ParserContextUtils.getType(ctx.typeType(), types);
            FField res = new FField(identifier, type, currentClass, visibilityModifier, statik);
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

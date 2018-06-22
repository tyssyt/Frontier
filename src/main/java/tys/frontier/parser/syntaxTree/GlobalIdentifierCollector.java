package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.*;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GlobalIdentifierCollector extends FrontierBaseVisitor {

    private Map<FClassIdentifier, FClass> classes;
    private SyntaxTreeData treeData;
    private List<SyntaxError> errors = new ArrayList<>();

    private FClass currentClass;

    private GlobalIdentifierCollector (FrontierParser.FileContext ctx) {
        treeData = new SyntaxTreeData(ctx);
        ctx.accept(this);
    }

    public static SyntaxTreeData getIdentifiers(FrontierParser.FileContext ctx, FFile file) throws SyntaxErrors {
        GlobalIdentifierCollector collector = new GlobalIdentifierCollector(ctx);
        file.setClasses(ImmutableMap.copyOf(collector.classes));
        if (!collector.errors.isEmpty())
            throw SyntaxErrors.create(collector.errors);
        return collector.treeData;
    }


    @Override
    public Object visitFile(FrontierParser.FileContext ctx) {
        classes = new HashMap<>(ctx.children.size());
        //first go over all classes once to know their names
        for (FrontierParser.ClassDeclarationContext c : ctx.classDeclaration()) {
            FClass clazz = ParserContextUtils.getClass(c);
            FClass old = classes.put(clazz.getIdentifier(), clazz);
            if (old != null) {
                errors.add(new IdentifierCollision(clazz, old));
            }
            treeData.classes.put(c, clazz);
        }
        //in the second pass find fields and methods headers
        return visitChildren(ctx);
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
    public Object visitParentClasses(FrontierParser.ParentClassesContext ctx) {
        for (TerminalNode parentClassCtx : ctx.TypeIdentifier()) {
            try {
                FClass parentClass = ParserContextUtils.getNonPredefined(parentClassCtx.getText(), classes);
                boolean changed = currentClass.addSuperClass(parentClass);
                if (!changed) {
                    errors.add(new ClassExtendedTwice(currentClass, parentClass));
                }
            } catch (ClassNotFound classNotFound) {
                errors.add(classNotFound);
            }
        }
        return null;
    }

    @Override
    public Object visitConstructorsDeclarative(FrontierParser.ConstructorsDeclarativeContext ctx) {
        currentClass.setConstructorVisibility(ParserContextUtils.getVisibility(ctx.visibilityModifier()));
        return null;
    }

    @Override
    public Object visitMethodHeader(FrontierParser.MethodHeaderContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        boolean statik = ParserContextUtils.isStatic(ctx.modifier());
        //return type
        FrontierParser.TypeTypeContext c = ctx.typeType();
        FClass returnType;
        if (c != null) {
            try {
                returnType = ParserContextUtils.getType(c, classes);
            } catch (ClassNotFound e) {
                errors.add(e);
                returnType = FVoid.INSTANCE; //TODO do we want some error related type here?
            }
        } else {
            returnType = FVoid.INSTANCE;
        }

        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
        try {
            ImmutableList<FParameter> params = formalParameters(ctx.formalParameters());
            FFunction res = new FFunction(identifier, currentClass, visibilityModifier, statik, returnType, params);
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
        List<ClassNotFound> errors = new ArrayList<>();
        for (FrontierParser.FormalParameterContext c : cs) {
            try {
                FParameter param = ParserContextUtils.getParameter(c.typedIdentifier(), classes);
                treeData.parameters.put(c, param);
                res.add(param);
            } catch (ClassNotFound e) {
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
            FLocalVariable var = ParserContextUtils.getVariable(ctx.variableDeclarator().typedIdentifier(), classes);
            FField res = new FField(var.getIdentifier(), var.getType(), currentClass, visibilityModifier, statik);
            currentClass.addField(res);
            treeData.fields.put(ctx, res);
        } catch (ClassNotFound | IdentifierCollision e) {
            errors.add(e);
        }
        return null;
    }
}

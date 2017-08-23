package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.*;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.parser.FrontierBaseVisitor;
import tys.frontier.parser.FrontierParser;
import tys.frontier.parser.syntaxTree.syntaxErrors.ClassNotFound;
import tys.frontier.parser.syntaxTree.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxTree.syntaxErrors.SignatureCollision;
import tys.frontier.parser.syntaxTree.syntaxErrors.SyntaxError;
import tys.frontier.util.Pair;

import java.util.*;

public class GlobalIdentifierCollector extends FrontierBaseVisitor {

    private Map<FClassIdentifier, FClass> classes;
    private SyntaxTreeData treeData;
    private List<SyntaxError> errors = new ArrayList<>();

    private FClass currentClass;

    private GlobalIdentifierCollector (FrontierParser.FileContext ctx) {
        treeData = new SyntaxTreeData(ctx);
        ctx.accept(this);
    }

    public static Pair<SyntaxTreeData, List<SyntaxError>> getIdentifiers (FFile file, FrontierParser.FileContext ctx) {
        GlobalIdentifierCollector collector = new GlobalIdentifierCollector(ctx);
        file.setClasses(ImmutableMap.copyOf(collector.classes));
        return new Pair<>(collector.treeData, collector.errors);
    }


    @Override
    public Object visitFile(FrontierParser.FileContext ctx) {
        classes = new HashMap<>(ctx.children.size());
        //first go over all classes once to know their names
        for (FrontierParser.ClassDeclarationContext c : ctx.classDeclaration()) {
            FClass clazz = ParserContextUtils.getClass(c);
            FClass old = classes.put(clazz.getIdentifier(), clazz);
            if (old != null) {
                errors.add(new IdentifierCollision(clazz, old, null));
            }
            treeData.classes.put(c, clazz);
        }
        //in the second pass find fields and methods headers
        return visitChildren(ctx);
    }

    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentClass = treeData.classes.get(ctx);
        for (FrontierParser.ClassBodyDeclarationContext c : ctx.classBodyDeclaration()) {
            c.accept(this);
        }
        return null;
    }

    @Override
    public Object visitMethodDeclaration(FrontierParser.MethodDeclarationContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        boolean statik = ParserContextUtils.isStatic(ctx.modifier());
        FrontierParser.TypeTypeContext c = ctx.typeType();
        FClass returnType;
        if (c != null) {
            Pair<FClass, Optional<ClassNotFound>> returnTypeAndError = ParserContextUtils.getType(c, classes);
            returnTypeAndError.b.ifPresent(errors::add);
            returnType = returnTypeAndError.a;
        } else {
            returnType = FVoid.INSTANCE;
        }
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
        Pair<List<FLocalVariable>, List<ClassNotFound>> params = ParserContextUtils.getParams(ctx.formalParameters(), classes);
        errors.addAll(params.b);

        FFunction res = new FFunction(identifier, currentClass, visibilityModifier, statik, returnType, params.a);
        try {
            currentClass.addFunction(res);
            treeData.functions.put(ctx, res);
        } catch (SignatureCollision e) {
            errors.add(e);
        }
        return null;
    }

    @Override
    public Object visitConstructorDeclaration(FrontierParser.ConstructorDeclarationContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        //TODO better error would be nice, but this will change anyway when we restrict identifiers in the grammar
        FClassIdentifier identifier = new FClassIdentifier(ctx.TypeIdentifier().getText());
        if (identifier != currentClass.getIdentifier())
            errors.add(new SyntaxError("invalid Identifier for constuctor: " + identifier + " in " + currentClass));
        Pair<List<FLocalVariable>, List<ClassNotFound>> paramsAndErrors =
                ParserContextUtils.getParams(ctx.formalParameters(), classes);
        errors.addAll(paramsAndErrors.b);


        FConstructor res = new FConstructor(visibilityModifier, currentClass, paramsAndErrors.a);
        try {
            currentClass.addFunction(res);
            treeData.constructors.put(ctx, res);
        } catch (SignatureCollision e) {
            errors.add(e);
        }
        return null;
    }

    @Override
    public Object visitFieldDeclaration(FrontierParser.FieldDeclarationContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        boolean statik = ParserContextUtils.isStatic(ctx.modifier());
        Pair<FLocalVariable, Optional<ClassNotFound>> variableAndError =
                ParserContextUtils.getVariable(ctx.variableDeclarator().typedIdentifier(), classes);
        variableAndError.b.ifPresent(errors::add);
        FLocalVariable v = variableAndError.a;

        FField res = new FField(v.getIdentifier(), v.getType(), currentClass, visibilityModifier, statik);
        try {
            currentClass.addField(res);
            treeData.fields.put(ctx, res);
        } catch (IdentifierCollision e) {
            errors.add(e);
        }
        return null;
    }
}

package tys.frontier.parser;

import tys.frontier.State;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.NativeDecl;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.module.FrontierModule;
import tys.frontier.code.module.Module;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.parser.syntaxTree.GlobalIdentifierCollector;
import tys.frontier.parser.syntaxTree.ParserContextUtils;
import tys.frontier.parser.syntaxTree.ToInternalRepresentation;
import tys.frontier.parser.warnings.Warning;
import tys.frontier.style.Style;
import tys.frontier.util.Pair;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static tys.frontier.logging.Logger.info;
import static tys.frontier.logging.Logger.warn;

public class Parser {

    private Set<FInstantiatedClass> classesToPrepare = new HashSet<>();

    public static FrontierModule parse(Path file, Style style) throws IOException, SyntaxErrors, SyntaxError {
        info("parsing  %s", file);
        Parser parser = new Parser();
        Parser old = State.get().setCurrentParser(parser);
        try {
            FrontierModule module = buildModule(file, style);
            resolveImports(module);
            collectTypes(module);
            Delegates delegates = collectMembers(module);
            Set<FInstantiatedClass> classesToPrepare = parser.classesToPrepare;
            parser.classesToPrepare = null;
            prepareClasses(module, delegates, classesToPrepare);
            visitBodies(module, delegates);
            info("finished %s", file);
            return module;
        } finally {
            old = State.get().setCurrentParser(old);
            assert old == parser;
        }
    }

    private static FrontierModule buildModule(Path entryPoint, Style style) throws IOException, SyntaxErrors, CyclicInclude, InvalidPath {
        return ModuleParser.buildModule(entryPoint, style);
    }

    private static void resolveImports(FrontierModule module) throws SyntaxError, SyntaxErrors {
        for (ParsedFile file : module.getFiles()) {
            for (String _import : file.findImports()) {
                Module importedModule = State.get().getImportResolver().requestModule(_import);
                file.addImport(importedModule);
            }
        }
    }

    private static void collectTypes(FrontierModule module) throws SyntaxErrors {
        List<SyntaxError> syntaxErrors = new ArrayList<>();
        for (ParsedFile file : module.getFiles()) {
            for (FrontierParser.ClassDeclarationContext ctx : file.getTreeData().root.classDeclaration()) {
                try {
                    FClass _class = createClass(file.getFilePath(), ctx);
                    DefaultNamespace old = file.resolveNamespace(_class.getIdentifier());
                    if (old != null)
                        throw new IdentifierCollision(_class.getNamespace(), old);
                    file.addNamespace(_class.getNamespace(), ctx);
                } catch (TwiceDefinedLocalVariable | IdentifierCollision e) {
                    syntaxErrors.add(e);
                }
            }
            for (FrontierParser.NamespaceDeclarationContext ctx : file.getTreeData().root.namespaceDeclaration()) {
                try {
                    DefaultNamespace namespace = createNamespace(file.getFilePath(), ctx);
                    DefaultNamespace old = file.resolveNamespace(namespace.getIdentifier());
                    if (old != null)
                        throw new IdentifierCollision(namespace, old);
                    file.addNamespace(namespace, ctx);
                } catch (IdentifierCollision e) {
                    syntaxErrors.add(e);
                }
            }
        }

        if (!syntaxErrors.isEmpty())
            throw SyntaxErrors.create(syntaxErrors);
    }

    private static Delegates collectMembers(FrontierModule module) throws SyntaxErrors {
        List<SyntaxError> syntaxErrors = new ArrayList<>();
        Delegates delegates = new Delegates();
        for (ParsedFile file : module.getFiles())
            GlobalIdentifierCollector.collectIdentifiers(file, delegates, syntaxErrors);

        if (!syntaxErrors.isEmpty())
            throw SyntaxErrors.create(syntaxErrors);
        return delegates;
    }

    private static void prepareClasses(FrontierModule module, Delegates delegates, Set<FInstantiatedClass> classesToPrepare) throws SyntaxErrors {
        //these steps need to be in exactly that order, because I prepare some in createDelegate and prepare needs contructors
        for (ParsedFile file : module.getFiles())
            for (DefaultNamespace namespace : file.getNamespaces().values())
                if (namespace.getType() != null)
                    namespace.getType().generateConstructor(false);
        delegates.createDelegatedFunctions(classesToPrepare);
        List<SyntaxError> errors = new ArrayList<>();
        for (FInstantiatedClass fInstantiatedClass : classesToPrepare) {
            try {
                fInstantiatedClass.prepare();
            } catch (NonEmbeddableType nonEmbeddableType) {
                errors.add(nonEmbeddableType);
            }
        }
        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }

    private static void visitBodies(FrontierModule module, Delegates delegates) throws SyntaxErrors {
        List<SyntaxError> syntaxErrors = new ArrayList<>();
        List<Warning> warnings = new ArrayList<>();
        for (ParsedFile file : module.getFiles())
            ToInternalRepresentation.toInternal(file, warnings, syntaxErrors);
        delegates.createDelegatedFunctionBodies();

        if (!syntaxErrors.isEmpty())
            throw SyntaxErrors.create(syntaxErrors);
        if (!warnings.isEmpty())
            warn(warnings.toString());
    }

    public void registerInstantiatedClass(FInstantiatedClass toRegister) throws NonEmbeddableType {
        if (classesToPrepare == null)
            toRegister.prepare();
        else
            classesToPrepare.add(toRegister);
    }

    public static FClass createClass(Path file, FrontierParser.ClassDeclarationContext ctx) throws TwiceDefinedLocalVariable {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        NativeDecl nativeDecl = ParserContextUtils.getNative(ctx.nativeModifier());
        FrontierParser.TypeParametersContext c = ctx.typeParameters();
        FBaseClass res = new FBaseClass(new Location(file, Position.fromCtx(ctx)), identifier, visibilityModifier, nativeDecl);
        res.addDefaultFunctions();
        if (c != null) {
            Pair<List<FTypeVariable>, List<Variance>> typeParameters = ParserContextUtils.getTypeParameters(c);
            res.setParameters(typeParameters.a, typeParameters.b);
        }
        return res;
    }

    public static DefaultNamespace createNamespace(Path file, FrontierParser.NamespaceDeclarationContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        return new DefaultNamespace(new Location(file, Position.fromCtx(ctx)), identifier, visibilityModifier, null, null);
    }

}

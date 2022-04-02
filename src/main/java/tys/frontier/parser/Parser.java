package tys.frontier.parser;

import com.google.common.collect.ImmutableMap;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.State;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.NativeDecl;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.module.FrontierModule;
import tys.frontier.code.module.Include;
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
import java.util.*;

import static com.google.common.io.MoreFiles.*;
import static java.nio.file.Files.isDirectory;
import static tys.frontier.logging.Logger.info;
import static tys.frontier.logging.Logger.warn;
import static tys.frontier.util.Utils.map;

public class Parser {

    private Set<FInstantiatedClass> classesToPrepare = new HashSet<>();

    public void registerInstantiatedClass(FInstantiatedClass toRegister) throws NonEmbeddableType {
        if (classesToPrepare == null)
            toRegister.prepare();
        else
            classesToPrepare.add(toRegister);
    }

    public static FrontierModule parse(Path file, Style style) throws IOException, SyntaxErrors, SyntaxError {
        info("parsing  %s", file);
        Parser parser = new Parser();
        Parser old = State.get().setCurrentParser(parser);
        try {
            var pair = findAllFiles(file, style);
            List<WipFile> files = pair.a;
            List<Module> imports = collectImports(files);
            List<DefaultNamespace> namespaces = collectTypes(files);
            Map<FIdentifier, DefaultNamespace> namespaceResolver = createNamespaceResolver(namespaces, imports);
            Delegates delegates = collectMembers(files, namespaceResolver);
            parser.prepareClasses(namespaces, delegates);
            visitBodies(files, delegates, namespaceResolver);
            info("finished %s", file);
            return new FrontierModule(file.toString(), imports, map(files, f -> f.path), pair.b, namespaces);
        } finally {
            old = State.get().setCurrentParser(old);
            assert old == parser;
        }
    }

    private static Pair<List<WipFile>, List<Include>> findAllFiles(Path entryPoint, Style style) throws IOException, SyntaxErrors, InvalidPath {
        List<WipFile> files = new ArrayList<>();
        List<Include> nativeIncludes = new ArrayList<>();

        Queue<Include> toDo = new ArrayDeque<>();
        toDo.add(new Include(entryPoint, false));

        queue: while (!toDo.isEmpty()) {
            Include cur = toDo.remove();
            assert !cur.out;

            for (WipFile file : files)
                if (file.path.equals(cur.path))
                    continue queue; // already visited

            FrontierParser.FileContext fileContext = Antlr.runAntlr(cur.path, style);
            files.add(new WipFile(cur.path, fileContext));

            var pair = collectIncludes(cur.path, fileContext.includeStatement());
            toDo.addAll(pair.a);
            nativeIncludes.addAll(pair.b);
        }

        return new Pair<>(files, nativeIncludes);
    }

    private static Pair<List<Include>, List<Include>> collectIncludes(Path fromFile, List<FrontierParser.IncludeStatementContext> ctxs) throws IOException, InvalidPath {
        List<Include> includes = new ArrayList<>();
        List<Include> nativeIncludes = new ArrayList<>();
        for (FrontierParser.IncludeStatementContext ctx : ctxs) {
            List<Include> res = ctx.NATIVE() != null ? nativeIncludes : includes;
            boolean out = ctx.OUT() != null;

            if (ctx.DOT() != null) {
                List<TerminalNode> ids = ctx.IDENTIFIER();
                String path = ids.get(0).getText() + '.' + ids.get(1).getText();
                res.add(new Include(fromFile.resolveSibling(path).normalize(), out));
            } else {
                String path = ParserContextUtils.getStringLiteral(ctx.StringLiteral().getSymbol());
                int starIndex = path.lastIndexOf('*');
                if (starIndex < 0) {
                    res.add(new Include(fromFile.resolveSibling(path).normalize(), out));
                } else {
                    boolean recursive = path.charAt(starIndex - 1) == '*';
                    Path folder = fromFile.resolveSibling(path.substring(0, recursive ? starIndex-1 : starIndex)).normalize();
                    Iterable<Path> pathIterable = recursive ? fileTraverser().breadthFirst(folder) : listFiles(folder);

                    String fileExtension;
                    if (path.length() == starIndex + 1)
                        fileExtension = null;
                    else if (path.charAt(starIndex + 1) == '.')
                        fileExtension = path.substring(starIndex + 2);
                    else
                        throw new InvalidPath(Position.fromCtx(ctx), path);

                    for (Path p : pathIterable)
                        if (!isDirectory(p) && (fileExtension == null || fileExtension.equals(getFileExtension(p))))
                            res.add(new Include(p, out));
                }
            }
        }
        return new Pair<>(includes, nativeIncludes);
    }

    private static List<Module> collectImports(List<WipFile> files) throws SyntaxError, SyntaxErrors {
        List<Module> imports = new ArrayList<>();
        for (WipFile file : files) {
            for (FrontierParser.ImportStatementContext ctx : file.treeData.root.importStatement()) {
                String _import = ctx.getChild(1).getText();
                Module importedModule = State.get().getImportResolver().requestModule(_import);
                imports.add(importedModule);
            }
        }
        return imports;
    }

    private static List<DefaultNamespace> collectTypes(List<WipFile> files) throws SyntaxErrors {
        List<DefaultNamespace> namespaces = new ArrayList<>();
        List<SyntaxError> syntaxErrors = new ArrayList<>();

        for (WipFile file : files) {
            for (FrontierParser.ClassDeclarationContext ctx : file.treeData.root.classDeclaration()) {
                try {
                    FClass _class = createClass(file.path, ctx);
                    file.treeData.classNamespaces.put(ctx, _class.getNamespace());
                    namespaces.add(_class.getNamespace());
                } catch (TwiceDefinedLocalVariable | PrivateNamespace e) {
                    syntaxErrors.add(e);
                }
            }
            for (FrontierParser.NamespaceDeclarationContext ctx : file.treeData.root.namespaceDeclaration()) {
                try {
                    DefaultNamespace namespace = createNamespace(file.path, ctx);
                    file.treeData.namespaces.put(ctx, namespace);
                    namespaces.add(namespace);
                } catch (PrivateNamespace e) {
                    syntaxErrors.add(e);
                }
            }
        }

        if (!syntaxErrors.isEmpty())
            throw SyntaxErrors.create(syntaxErrors);
        return namespaces;
    }

    private static ImmutableMap<FIdentifier, DefaultNamespace> createNamespaceResolver(List<DefaultNamespace> namespaces, List<Module> imports) throws SyntaxErrors {
        ImmutableMap.Builder<FIdentifier, DefaultNamespace> builder = ImmutableMap.builder();

        for (DefaultNamespace namespace : namespaces)
            builder.put(namespace.getIdentifier(), namespace);

        for (Module _import : imports)
            builder.putAll(_import.getExportedNamespaces());

        try {
            return builder.build();
        } catch (IllegalArgumentException e) {
            // there was a duplicate, find it! The builder doesn't let us see its contents which sucks
            List<SyntaxError> syntaxErrors = new ArrayList<>();
            Map<FIdentifier, DefaultNamespace> seen = new HashMap<>();

            for (DefaultNamespace namespace : namespaces) {
                DefaultNamespace old = seen.put(namespace.getIdentifier(), namespace);
                if (old != null)
                    syntaxErrors.add(new IdentifierCollision(old, namespace));
            }

            for (Module _import : imports) {
                for (DefaultNamespace namespace : _import.getExportedNamespaces().values()) {
                    DefaultNamespace old = seen.put(namespace.getIdentifier(), namespace);
                    if (old != null)
                        syntaxErrors.add(new IdentifierCollision(old, namespace));
                }
            }

            throw SyntaxErrors.create(syntaxErrors);
        }
    }

    private static Delegates collectMembers(List<WipFile> files, Map<FIdentifier, DefaultNamespace> namespaceResolver) throws SyntaxErrors {
        List<SyntaxError> syntaxErrors = new ArrayList<>();
        Delegates delegates = new Delegates();
        for (WipFile file : files)
            GlobalIdentifierCollector.collectIdentifiers(file.treeData, namespaceResolver, delegates, syntaxErrors);

        if (!syntaxErrors.isEmpty())
            throw SyntaxErrors.create(syntaxErrors);
        return delegates;
    }

    private void prepareClasses(List<DefaultNamespace> namespaces, Delegates delegates) throws SyntaxErrors {
        Set<FInstantiatedClass> prepare = classesToPrepare;
        classesToPrepare = null;

        //these steps need to be in exactly that order, because I prepare some in createDelegate and prepare needs contructors
        for (DefaultNamespace namespace : namespaces)
            if (namespace.getType() != null)
                namespace.getType().generateConstructor(false);
        delegates.createDelegatedFunctions(prepare);
        List<SyntaxError> errors = new ArrayList<>();
        for (FInstantiatedClass fInstantiatedClass : prepare) {
            try {
                fInstantiatedClass.prepare();
            } catch (NonEmbeddableType nonEmbeddableType) {
                errors.add(nonEmbeddableType);
            }
        }
        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }

    private static void visitBodies(List<WipFile> files, Delegates delegates, Map<FIdentifier, DefaultNamespace> namespaceResolver) throws SyntaxErrors {
        List<SyntaxError> syntaxErrors = new ArrayList<>();
        List<Warning> warnings = new ArrayList<>();
        for (WipFile file : files)
            ToInternalRepresentation.toInternal(file.treeData, namespaceResolver, warnings, syntaxErrors);
        delegates.createDelegatedFunctionBodies();

        if (!syntaxErrors.isEmpty())
            throw SyntaxErrors.create(syntaxErrors);
        if (!warnings.isEmpty())
            warn(warnings.toString());
    }

    private static FClass createClass(Path file, FrontierParser.ClassDeclarationContext ctx) throws TwiceDefinedLocalVariable, PrivateNamespace {
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());

        if (visibilityModifier == FVisibilityModifier.PRIVATE)
            throw new PrivateNamespace(Position.fromCtx(ctx), identifier);

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

    private static DefaultNamespace createNamespace(Path file, FrontierParser.NamespaceDeclarationContext ctx) throws PrivateNamespace {
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());

        if (visibilityModifier == FVisibilityModifier.PRIVATE)
            throw new PrivateNamespace(Position.fromCtx(ctx), identifier);

        return new DefaultNamespace(new Location(file, Position.fromCtx(ctx)), identifier, visibilityModifier, null, null);
    }

}

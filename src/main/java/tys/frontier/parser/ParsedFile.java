package tys.frontier.parser;

import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.module.FrontierModule;
import tys.frontier.code.module.Include;
import tys.frontier.code.module.Module;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.InvalidPath;
import tys.frontier.parser.syntaxTree.ParserContextUtils;
import tys.frontier.parser.syntaxTree.SyntaxTreeData;
import tys.frontier.util.Pair;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.google.common.io.MoreFiles.*;
import static java.nio.file.Files.isDirectory;

public class ParsedFile {

    private SyntaxTreeData treeData; //TODO this is only needed in the initial parsing and should maybe be stored on parser level instead

    private Path filePath;
    private FrontierModule module;

    private ParsedFile parent;
    private List<ParsedFile> includes = new ArrayList<>();
    private List<Module> imports = new ArrayList<>();
    private Map<FIdentifier, DefaultNamespace> namespaces = new LinkedHashMap<>();

    public ParsedFile(FrontierParser.FileContext fileContext, Path filePath) {
        this.treeData = new SyntaxTreeData(fileContext);
        this.filePath = filePath;
    }

    public void setParent(ParsedFile parent) {
        assert module == null;
        this.parent = parent;
        this.module = parent.module;
    }

    public void setModule(FrontierModule module) {
        assert this.module == null;
        this.module = module;
    }

    public Path getFilePath() {
        return filePath;
    }

    public ParsedFile getParent() {
        return parent;
    }

    public void addInclude(ParsedFile include) {
        includes.add(include);
    }

    public void addImport(Module _import) {
        imports.add(_import);
    }

    public DefaultNamespace addNamespace(DefaultNamespace namespace, FrontierParser.ClassDeclarationContext ctx) {
        treeData.classNamespaces.put(ctx, namespace);
        return namespaces.put(namespace.getIdentifier(), namespace);
    }

    public DefaultNamespace addNamespace(DefaultNamespace namespace, FrontierParser.NamespaceDeclarationContext ctx) {
        treeData.namespaces.put(ctx, namespace);
        return namespaces.put(namespace.getIdentifier(), namespace);
    }

    public Map<FIdentifier, DefaultNamespace> getNamespaces() {
        return namespaces;
    }

    public List<ParsedFile> getIncludes() {
        return includes;
    }

    public List<Module> getImports() {
        return imports;
    }

    public List<String> findImports() {
        List<FrontierParser.ImportStatementContext> ctxs = treeData.root.importStatement();
        List<String> res = new ArrayList<>(ctxs.size());
        for (FrontierParser.ImportStatementContext ctx : ctxs) {
            res.add(ctx.getChild(1).getText());
        }
        return res;
    }

    public Pair<List<Include>, List<Include>> findIncludes() throws IOException, InvalidPath {
        List<FrontierParser.IncludeStatementContext> ctxs = treeData.root.includeStatement();
        List<Include> includes = new ArrayList<>();
        List<Include> nativeIncludes = new ArrayList<>();
        for (FrontierParser.IncludeStatementContext ctx : ctxs) {
            List<Include> res = ctx.NATIVE() != null ? nativeIncludes : includes;
            boolean out = ctx.OUT() != null;

            if (ctx.DOT() != null) {
                List<TerminalNode> ids = ctx.IDENTIFIER();
                String path = ids.get(0).getText() + '.' + ids.get(1).getText();
                res.add(new Include(filePath.resolveSibling(path).normalize(), out));
            } else {
                String path = ParserContextUtils.getStringLiteral(ctx.StringLiteral().getSymbol());
                int starIndex = path.lastIndexOf('*');
                if (starIndex < 0) {
                    res.add(new Include(filePath.resolveSibling(path).normalize(), out));
                } else {
                    boolean recursive = path.charAt(starIndex - 1) == '*';
                    Path folder = filePath.resolveSibling(path.substring(0, recursive ? starIndex-1 : starIndex)).normalize();
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

    public SyntaxTreeData getTreeData() {
        return treeData;
    }

    //TODO this does not work multithreaded if any of the parents is worked on!
    public DefaultNamespace resolveNamespace(FIdentifier identifier) {
        DefaultNamespace res = namespaces.get(identifier); //allows private classes
        if (res != null)
            return res;

        res = module.getNamespace(identifier); //does not check private classes
        if (res != null)
            return res;


        ParsedFile cur = this;

        while (cur != null) {
            for (Module _import : cur.getImports()) {
                res = _import.getExportedNamespaces().get(identifier);
                if (res != null)
                    return res;
            }

            cur = cur.getParent();
        }
        return null;
    }
}

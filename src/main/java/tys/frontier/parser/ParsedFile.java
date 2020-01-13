package tys.frontier.parser;

import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.module.Module;
import tys.frontier.code.type.FClass;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxTree.SyntaxTreeData;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

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
    private Module module;

    private ParsedFile parent;
    private List<ParsedFile> includes = new ArrayList<>();
    private List<Module> imports = new ArrayList<>();
    private Map<FIdentifier, FClass> classes = new LinkedHashMap<>();

    public ParsedFile(FrontierParser.FileContext fileContext, Path filePath) {
        this.treeData = new SyntaxTreeData(fileContext);
        this.filePath = filePath;
    }

    public void setParent(ParsedFile parent) {
        assert module == null;
        this.parent = parent;
        this.module = parent.module;
    }

    public void setModule(Module module) {
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

    public FClass getClass(FIdentifier identifier) {
        return classes.get(identifier);
    }

    public FClass addClass(FClass _class, FrontierParser.ClassDeclarationContext ctx) {
        treeData.classes.put(ctx, _class);
        return classes.put(_class.getIdentifier(), _class);
    }

    public Map<FIdentifier, FClass> getClasses() {
        return classes;
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
            res.add(ctx.IDENTIFIER().getText());
        }
        return res;
    }

    public Pair<List<Path>, List<Path>> findIncludes() throws IOException {
        List<FrontierParser.IncludeStatementContext> ctxs = treeData.root.includeStatement();
        List<Path> includes = new ArrayList<>();
        List<Path> nativeIncludes = new ArrayList<>();
        for (FrontierParser.IncludeStatementContext ctx : ctxs) {
            List<Path> res = ctx.NATIVE() != null ? nativeIncludes : includes;
            FrontierParser.PathContext pathContext = ctx.path();
            if (pathContext instanceof FrontierParser.FilePathContext) {
                res.add(filePath.resolveSibling(pathContext.getText()).normalize());
            } else if (pathContext instanceof FrontierParser.FolderPathContext) {
                FrontierParser.FolderPathContext folderPathContext = (FrontierParser.FolderPathContext) pathContext;

                Path folder = filePath.resolveSibling(folderPathContext.folder().getText()).normalize();
                boolean recursive = folderPathContext.STAR().size() == 2;
                Iterable<Path> pathIterable = recursive ? fileTraverser().breadthFirst(folder) : listFiles(folder);

                TerminalNode identifierContext = folderPathContext.IDENTIFIER();
                String fileExtension = identifierContext == null ? null : identifierContext.getText();
                for (Path path : pathIterable)
                    if (!isDirectory(path) && (fileExtension == null || fileExtension.equals(getFileExtension(path))))
                        res.add(path);
            } else {
                return Utils.cantHappen();
            }
        }
        return new Pair<>(includes, nativeIncludes);
    }

    public SyntaxTreeData getTreeData() {
        return treeData;
    }

    //TODO this does not work multithreaded if any of the parents is worked on!
    public FClass resolveType(FIdentifier identifier) {
        FClass res = classes.get(identifier); //allows private classes
        if (res != null)
            return res;

        res = module.getClass(identifier); //does not check private classes
        if (res != null)
            return res;


        ParsedFile cur = this;

        while (cur != null) {
            for (Module _import : cur.getImports()) {
                res = _import.getExportedClasses().get(identifier);
                if (res != null)
                    return res;
            }

            cur = cur.getParent();
        }
        return null;
    }
}

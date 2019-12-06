package tys.frontier.parser;

import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.module.Module;
import tys.frontier.code.type.FClass;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxTree.SyntaxTreeData;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class ParsedFile {

    private SyntaxTreeData treeData; //TODO this is only needed in the initial parsing and should maybe be stored on parser level instead

    private String fileName;
    private Module module;

    private ParsedFile parent;
    private List<ParsedFile> includes = new ArrayList<>();
    private List<Module> imports = new ArrayList<>();
    private Map<FTypeIdentifier, FClass> classes = new LinkedHashMap<>();

    public ParsedFile(FrontierParser.FileContext fileContext, String fileName) {
        this.treeData = new SyntaxTreeData(fileContext);
        this.fileName = fileName;
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

    public String getFileName() {
        return fileName;
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

    public FClass getClass(FTypeIdentifier identifier) {
        return classes.get(identifier);
    }

    public FClass addClass(FClass _class, FrontierParser.ClassDeclarationContext ctx) {
        treeData.classes.put(ctx, _class);
        return classes.put(_class.getIdentifier(), _class);
    }

    public Map<FTypeIdentifier, FClass> getClasses() {
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
            res.add(ctx.identifier().getChild(0).getText());
        }
        return res;
    }

    public List<String> findIncludes() {
        List<FrontierParser.IncludeStatementContext> ctxs = treeData.root.includeStatement();
        List<String> res = new ArrayList<>(ctxs.size());
        for (FrontierParser.IncludeStatementContext ctx : ctxs) {
            res.add(ctx.identifier().getChild(0).getText());
        }
        return res;
    }

    public SyntaxTreeData getTreeData() {
        return treeData;
    }

    //TODO this does not work multithreaded if any of the parents is worked on!
    public FClass resolveType(FTypeIdentifier identifier) {
        ParsedFile cur = this;

        while (cur != null) {
            FClass res = cur.getClass(identifier);
            if (res != null)
                return res;

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

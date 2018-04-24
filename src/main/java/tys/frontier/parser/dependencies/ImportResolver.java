package tys.frontier.parser.dependencies;

import tys.frontier.code.module.Module;
import tys.frontier.modules.io.IOModule;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.parser.syntaxErrors.UnresolvableImport;

import java.util.ArrayList;
import java.util.List;

public class ImportResolver extends FrontierBaseVisitor {

    private List<Module> dependencies = new ArrayList<>();
    private List<UnresolvableImport> unresolvableImports = new ArrayList<>();

    public static List<Module> resolve(FrontierParser.FileContext ctx) throws SyntaxErrors {
        ImportResolver importResolver = new ImportResolver();
        importResolver.visitFile(ctx);
        if (!importResolver.unresolvableImports.isEmpty()) {
            throw new SyntaxErrors(importResolver.unresolvableImports);
        }
        return importResolver.dependencies;
    }

    private ImportResolver() {
    }

    @Override
    public Object visitImportStatement(FrontierParser.ImportStatementContext ctx) {
        String moduleName = ctx.ModuleIdentifier().getText();
        try {
            dependencies.add(resolveImport(moduleName));
        } catch (UnresolvableImport unresolvableImport) {
            unresolvableImports.add(unresolvableImport);
        }
        return null;
    }

    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        return null;
    }

    private Module resolveImport(String name) throws UnresolvableImport {
        if (name.equals("IO")) { //super advanced resolving methods right here
            return IOModule.INSTANCE;
        }
        throw new UnresolvableImport(name);
    }

}

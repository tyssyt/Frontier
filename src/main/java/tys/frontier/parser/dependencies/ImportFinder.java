package tys.frontier.parser.dependencies;

import tys.frontier.code.module.Module;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;

import java.util.ArrayList;
import java.util.List;

public class ImportFinder extends FrontierBaseVisitor {

    private ImportResolver resolver;
    private List<Module> dependencies = new ArrayList<>();
    private List<SyntaxError> errors = new ArrayList<>();

    public static List<Module> resolve(FrontierParser.FileContext ctx, ImportResolver resolver) throws SyntaxErrors {
        ImportFinder importFinder = new ImportFinder(resolver);
        importFinder.visitFile(ctx);
        if (!importFinder.errors.isEmpty()) {
            throw SyntaxErrors.create(importFinder.errors);
        }
        return importFinder.dependencies;
    }

    public ImportFinder(ImportResolver resolver) {
        this.resolver = resolver;
    }

    @Override
    public Object visitImportStatement(FrontierParser.ImportStatementContext ctx) {
        String moduleName = ctx.TypeIdentifier().getText();
        try {
            dependencies.add(resolver.requestModule(moduleName));
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
        } catch (SyntaxErrors syntaxErrors) {
            errors.addAll(syntaxErrors.errors);
        }
        return null;
    }

    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        return null;
    }

}

package tys.frontier.parser;

import tys.frontier.State;
import tys.frontier.code.module.Module;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.logging.Log;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.parser.syntaxTree.GlobalIdentifierCollector;
import tys.frontier.parser.syntaxTree.ParserContextUtils;
import tys.frontier.parser.syntaxTree.ToInternalRepresentation;
import tys.frontier.parser.warnings.Warning;
import tys.frontier.style.Style;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Parser {

    private Set<FInstantiatedClass> classesToPrepare = new HashSet<>();

    public static Module parse(Path file, Style style) throws IOException, SyntaxErrors, CyclicModuleDependency, UnresolvableImport {
        Parser parser = new Parser();
        Parser old = State.get().setCurrentParser(parser);
        try {
            Module module = buildModule(file, style);
            resolveImports(module);
            collectTypes(module);
            Delegates delegates = collectMembers(module);
            Set<FInstantiatedClass> classesToPrepare = parser.classesToPrepare;
            parser.classesToPrepare = null;
            prepareClasses(module, delegates, classesToPrepare);
            visitBodies(module, delegates);
            return module;
        } finally {
            old = State.get().setCurrentParser(old);
            assert old == parser;
        }
    }

    private static Module buildModule(Path entryPoint, Style style) throws IOException, SyntaxErrors {
        return ModuleParser.buildModule(entryPoint, style);
    }

    private static void resolveImports(Module module) throws CyclicModuleDependency, UnresolvableImport, SyntaxErrors {
        for (ParsedFile file : module.getFiles()) {
            for (String _import : file.findImports()) {
                Module importedModule = State.get().getImportResolver().requestModule(_import);
                file.addImport(importedModule);
            }
        }
    }

    private static void collectTypes(Module module) throws SyntaxErrors {
        List<SyntaxError> syntaxErrors = new ArrayList<>();
        for (ParsedFile file : module.getFiles()) {
            for (FrontierParser.ClassDeclarationContext ctx : file.getTreeData().root.classDeclaration()) {
                try {
                    FClass _class = ParserContextUtils.getClass(ctx);
                    FType old = file.resolveType(_class.getIdentifier());
                    if (old != null)
                        throw new IdentifierCollision(_class, old);
                    file.addClass(_class, ctx);
                } catch (TwiceDefinedLocalVariable | IdentifierCollision e) {
                    syntaxErrors.add(e);
                }
            }
        }

        if (!syntaxErrors.isEmpty())
            throw SyntaxErrors.create(syntaxErrors);
    }

    private static Delegates collectMembers(Module module) throws SyntaxErrors {
        List<SyntaxError> syntaxErrors = new ArrayList<>();
        Delegates delegates = new Delegates();
        for (ParsedFile file : module.getFiles())
            GlobalIdentifierCollector.collectIdentifiers(file, delegates, syntaxErrors);

        if (!syntaxErrors.isEmpty())
            throw SyntaxErrors.create(syntaxErrors);
        return delegates;
    }

    private static void prepareClasses(Module module, Delegates delegates, Set<FInstantiatedClass> classesToPrepare) throws SyntaxErrors {
        //these steps need to be in exactly that order, because I prepare some in createDelegate and prepare needs contructors
        for (ParsedFile file : module.getFiles())
            file.getClasses().values().forEach(FClass::generateConstructor);
        delegates.createDelegatedFunctions(classesToPrepare);
        classesToPrepare.forEach(FInstantiatedClass::prepare);
    }

    private static void visitBodies(Module module, Delegates delegates) throws SyntaxErrors {
        List<SyntaxError> syntaxErrors = new ArrayList<>();
        List<Warning> warnings = new ArrayList<>();
        for (ParsedFile file : module.getFiles())
            ToInternalRepresentation.toInternal(file, warnings, syntaxErrors);
        delegates.createDelegatedFunctionBodies();

        if (!syntaxErrors.isEmpty())
            throw SyntaxErrors.create(syntaxErrors);
        if (!warnings.isEmpty())
            Log.warning(Parser.class, warnings.toString());
    }

    public void registerInstantiatedClass(FInstantiatedClass toRegister) {
        if (classesToPrepare == null)
            toRegister.prepare();
        else
            classesToPrepare.add(toRegister);
    }

}

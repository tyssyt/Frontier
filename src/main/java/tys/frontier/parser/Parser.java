package tys.frontier.parser;

import com.google.common.collect.MoreCollectors;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.module.Module;
import tys.frontier.logging.Log;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.dependencies.ImportFinder;
import tys.frontier.parser.dependencies.ImportResolver;
import tys.frontier.parser.syntaxErrors.AntRecognitionException;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.parser.syntaxTree.GlobalIdentifierCollector;
import tys.frontier.parser.syntaxTree.SyntaxTreeData;
import tys.frontier.parser.syntaxTree.ToInternalRepresentation;
import tys.frontier.parser.warnings.Warning;
import tys.frontier.style.Style;
import tys.frontier.util.Utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.NoSuchElementException;

public class Parser {

    public enum Stage {
        CREATED,
        INITIALIZING,
        IMPORT_RESOLVING,
        IDENTIFIER_COLLECTION,
        IDENTIFIER_CHECKS,
        TO_INTERNAL_REPRESENTATION,
        CHECKS,
        FINISHED;

        @Override
        public String toString() {
            return this.name().toLowerCase();
        }
    }

    private String file;
    private Style style;
    private ImportResolver importResolver;

    private Stage stage = Stage.CREATED;

    public Parser(String file, Style style) {
        this.file = file;
        this.style = style;
        this.importResolver = new ImportResolver(style);
    }

    public Parser(String file, Style style, ImportResolver importResolver) {
        this.file = file;
        this.style = style;
        this.importResolver = importResolver;
    }

    public Stage getStage() {
        return stage;
    }

    public Module parse() throws SyntaxErrors, IOException {
        assert stage == Stage.CREATED;
        stage = Stage.INITIALIZING;
        Module res = new Module(file, "1", null); //TODO properly extract this info from the module

        if (!file.endsWith(".front")) {
            file = "Frontier Libs/" + file + ".front";
        }

        //create Lexer & Parser & parse
        try (InputStream input = Utils.loadFile(file)) {
            FrontierParser.FileContext context;
            {
                FrontierLexer lexer = new FrontierLexer(CharStreams.fromStream(input), style.getKeywords());
                CommonTokenStream tokens = new CommonTokenStream(lexer);
                FrontierParser parser = new FrontierParser(tokens);
                AntLrErrorListener errorListener = new AntLrErrorListener();
                parser.addErrorListener(errorListener);
                context = parser.file();
                if (parser.getNumberOfSyntaxErrors() > 0)
                    throw SyntaxErrors.create(errorListener.errors);
            }

            stage = Stage.IMPORT_RESOLVING;
            res.getImportedModules().addAll(ImportFinder.resolve(context, importResolver));

            stage = Stage.IDENTIFIER_COLLECTION;
            SyntaxTreeData treeData = GlobalIdentifierCollector.getIdentifiers(context);
            for (FClass fClass : treeData.classes.values()) {
                res.addClass(fClass);
            }
            {
                Log.info(this, "parsed identifiers");
                Log.debug(this, res.toString());
            }

            stage = Stage.IDENTIFIER_CHECKS;

            stage = Stage.TO_INTERNAL_REPRESENTATION;
            List<Warning> warnings = ToInternalRepresentation.toInternal(treeData, res);
            {
                Log.info(this, "parsed classes");
                Log.debug(this, res.toString());
                if (!warnings.isEmpty()) {
                    Log.warning(this, warnings.toString());
                }
            }

            /*
            stage = Stage.CHECKS;
            Log.info(this, "checks passed");
            */

            stage = Stage.FINISHED;
            //search for entry Point
            try {
                FFunction entryPoint = res.getExportedClasses().values().stream()
                        .flatMap(cl -> cl.getStaticFunctions().values().stream())
                        .filter(FFunction::isMain)
                        .collect(MoreCollectors.onlyElement());
                res.setEntryPoint(entryPoint);
            } catch (IllegalArgumentException e) {
                Log.warning(this, "more then 1 entry Point found in File", e);
            } catch (NoSuchElementException e) {
                Log.info(this, "no entry Point found in File", e);
            }
            return res;
        }
    }

    @Override
    public String toString() {
        return "Parser{file='" + file + ", " + stage + '}';
    }

    private class AntLrErrorListener implements ANTLRErrorListener {

        private List<AntRecognitionException> errors = new ArrayList<>();

        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
            errors.add(new AntRecognitionException(e));
        }

        @Override
        public void reportAmbiguity(org.antlr.v4.runtime.Parser recognizer, DFA dfa, int startIndex, int stopIndex, boolean exact, BitSet ambigAlts, ATNConfigSet configs) {

        }

        @Override
        public void reportAttemptingFullContext(org.antlr.v4.runtime.Parser recognizer, DFA dfa, int startIndex, int stopIndex, BitSet conflictingAlts, ATNConfigSet configs) {

        }

        @Override
        public void reportContextSensitivity(org.antlr.v4.runtime.Parser recognizer, DFA dfa, int startIndex, int stopIndex, int prediction, ATNConfigSet configs) {

        }
    }
}

package tys.frontier.parser;

import com.google.common.collect.MoreCollectors;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.apache.commons.logging.LogFactory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.module.Module;
import tys.frontier.code.type.FClass;
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
import tys.frontier.util.Pair;
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

    Logger logger = LogManager.getLogger(Parser.class);

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
            Pair<SyntaxTreeData, Delegates> treeDataAndDelegates = GlobalIdentifierCollector.getIdentifiers(context, res);
            SyntaxTreeData treeData = treeDataAndDelegates.a;
            treeDataAndDelegates.b.createDelegatedFunctions();
            for (FClass fClass : treeData.classes.values()) {
                res.addClass(fClass);
            }
            {
                logger.info("parsed identifiers");
                logger.debug(res.toString());
            }

            stage = Stage.IDENTIFIER_CHECKS;

            stage = Stage.TO_INTERNAL_REPRESENTATION;
            List<Warning> warnings = ToInternalRepresentation.toInternal(treeData, res);
            treeDataAndDelegates.b.createDelegatedFunctionBodies();
            {
                logger.info("parsed classes");
                logger.debug(res.toString());
                if (!warnings.isEmpty()) {
                    logger.warn(warnings.toString());
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
                        .flatMap(cl -> cl.getFunctions().values().stream())
                        .filter(FFunction::isMain)
                        .collect(MoreCollectors.onlyElement());
                res.setEntryPoint(entryPoint);
            } catch (IllegalArgumentException e) {
                logger.warn("more then 1 entry Point found in File", e);
            } catch (NoSuchElementException e) {
                logger.info("no entry Point found in File", e);
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

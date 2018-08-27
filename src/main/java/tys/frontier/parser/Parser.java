package tys.frontier.parser;

import com.google.common.collect.MoreCollectors;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import tys.frontier.code.FFile;
import tys.frontier.code.FFunction;
import tys.frontier.code.module.FrontierModule;
import tys.frontier.logging.Log;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.dependencies.ImportResolver;
import tys.frontier.parser.semanticAnalysis.CheckNamespaces;
import tys.frontier.parser.semanticAnalysis.CyclicClassHierachyCheck;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.AntRecognitionException;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.parser.syntaxTree.GlobalIdentifierCollector;
import tys.frontier.parser.syntaxTree.RemoveAbstractConstructors;
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

    private String file;
    private Style style;

    private Stage stage = Stage.CREATED;

    public Parser(String file, Style style) {
        this.file = file;
        this.style = style;
    }

    public Stage getStage() {
        return stage;
    }

    public FrontierModule parse() throws IOException, SyntaxErrors {
        assert stage == Stage.CREATED;
        stage = Stage.INITIALIZING;
        FrontierModule res = new FrontierModule("Anonymous", "1", null);

        //create Lexer & Parser & parse
        FrontierLexer lexer;
        try (InputStream input = Utils.loadFile(file)) {
            lexer = new FrontierLexer(CharStreams.fromStream(input), style.getKeywords());
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            FrontierParser parser = new FrontierParser(tokens);
            AntLrErrorListener errorListener = new AntLrErrorListener();
            parser.addErrorListener(errorListener);
            FrontierParser.FileContext context = parser.file();

            if (parser.getNumberOfSyntaxErrors() > 0)
                throw SyntaxErrors.create(errorListener.errors);

            stage = Stage.IMPORT_RESOLVING;
            res.addDependencies(ImportResolver.resolve(context));

            stage = Stage.IDENTIFIER_COLLECTION;
            FFile file = new FFile(this.file);
            SyntaxTreeData treeData = GlobalIdentifierCollector.getIdentifiers(context, file);
            {
                Log.info(this, "parsed identifiers");
                Log.debug(this, file.summary());
            }

            stage= Stage.IDENTIFIER_CHECKS;
            res.addFile(file);
            //check this early because cyclic class hierachies would create nontermination in function resolving etc.
            CyclicClassHierachyCheck.check(res.getInternalClassHierachy());

            stage = Stage.TO_INTERNAL_REPRESENTATION;
            Pair<List<NeedsTypeCheck>, List<Warning>> typeChecksAndWarnings = ToInternalRepresentation.toInternal(treeData, file, res.getImportedClasses());
            RemoveAbstractConstructors.remove(res.getInternalClassHierachy());
            {
                Log.info(this, "parsed classes");
                Log.debug(this, file.toString());
                if (!typeChecksAndWarnings.b.isEmpty()) {
                    Log.warning(this, typeChecksAndWarnings.b.toString());
                }
            }

            stage = Stage.CHECKS;
            NeedsTypeCheck.checkAll(typeChecksAndWarnings.a);
            CheckNamespaces.check(res.getInternalClassHierachy()); //TODO namespace check should extend past local class hierachy
            //TODO namespace check should ideally be between GlobalIdentifierCollector and toInternal, but it needs override info which is computet at the beginning of toInternal
            Log.info(this, "checks passed");

            stage = Stage.FINISHED;
            //search for entry Point
            try {
                FFunction entryPoint = res.getExportedFunctions().values().stream()
                        .filter(FFunction::isMain)
                        .collect(MoreCollectors.onlyElement());
                res.setEntryPoint(entryPoint);
            } catch (IllegalArgumentException e) {
                Log.warning(this, "more then 1 entry Point found in File", e);
            } catch (NoSuchElementException e) {
                Log.warning(this, "no entry Point found in File", e);
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

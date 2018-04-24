package tys.frontier.parser;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import tys.frontier.code.FFile;
import tys.frontier.code.module.FrontierModule;
import tys.frontier.logging.Log;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.dependencies.ImportResolver;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.AntRecognitionException;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.parser.syntaxTree.GlobalIdentifierCollector;
import tys.frontier.parser.syntaxTree.SyntaxTreeData;
import tys.frontier.parser.syntaxTree.ToInternalRepresentation;
import tys.frontier.style.Style;
import tys.frontier.util.Utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

public class Parser {

    public enum Stage {
        CREATED,
        INITIALIZING,
        IMPORT_RESOLVING,
        IDENTIFIER_COLLECTION,
        TO_INTERNAL_REPRESENTATION,
        TYPE_CHECKS,
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
                throw new SyntaxErrors(errorListener.errors);

            stage = Stage.IMPORT_RESOLVING;
            res.addDependencies(ImportResolver.resolve(context));

            stage = Stage.IDENTIFIER_COLLECTION;
            FFile file = new FFile(this.file);
            SyntaxTreeData treeData = GlobalIdentifierCollector.getIdentifiers(context, file);
            {
                StringBuilder sb = new StringBuilder().append("parsed identifiers:\n");
                file.summary(sb);
                Log.info(this, sb.toString());
            }

            stage = Stage.TO_INTERNAL_REPRESENTATION;
            List<NeedsTypeCheck> typeChecks = ToInternalRepresentation.toInternal(treeData, file, res.getImportedClasses());
            {
                StringBuilder sb = new StringBuilder().append("parsed classes:\n");
                file.toString(sb);
                Log.info(this, sb.toString());
            }

            stage = Stage.TYPE_CHECKS;
            NeedsTypeCheck.checkAll(typeChecks);
            Log.info(this, "typecheck passed");

            stage = Stage.FINISHED;
            res.addFile(file);
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

package tys.frontier.parser;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.AntRecognitionException;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.style.Style;
import tys.frontier.util.Utils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

public class FileParser {

    public static ParsedFile runAntlr(Path file, Style style) throws IOException, SyntaxErrors {
        try (InputStream input = Utils.loadFile(file.toString())) {
            FrontierLexer lexer = new FrontierLexer(CharStreams.fromStream(input), style.getKeywords());
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            FrontierParser parser = new FrontierParser(tokens);
            AntLrErrorListener errorListener = new AntLrErrorListener();
            parser.addErrorListener(errorListener);
            FrontierParser.FileContext res = parser.file();
            if (parser.getNumberOfSyntaxErrors() > 0)
                throw SyntaxErrors.create(errorListener.errors);
            return new ParsedFile(res, file);
        }
    }

    private static class AntLrErrorListener implements ANTLRErrorListener {

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

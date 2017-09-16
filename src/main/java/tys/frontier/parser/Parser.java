package tys.frontier.parser;

import com.opensymphony.xwork2.util.ClassLoaderUtil;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import tys.frontier.code.FFile;
import tys.frontier.code.statement.NeedsTypeCheck;
import tys.frontier.logging.Log;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxTree.GlobalIdentifierCollector;
import tys.frontier.parser.syntaxTree.SyntaxTreeData;
import tys.frontier.parser.syntaxTree.ToInternalRepresentation;
import tys.frontier.parser.syntaxTree.syntaxErrors.SyntaxErrors;
import tys.frontier.style.Style;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

public class Parser {

    public enum Stage {
        CREATED,
        INITIALIZING,
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

    public FFile parse() throws IOException, SyntaxErrors {
        assert stage == Stage.CREATED;
        stage = Stage.INITIALIZING;
        //create Lexer & Parser & parse
        FrontierLexer lexer;
        InputStream input = ClassLoaderUtil.getResourceAsStream(file, this.getClass());
        lexer = new FrontierLexer(CharStreams.fromStream(input), style.getKeywords());
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        FrontierParser parser = new FrontierParser(tokens);
        FrontierParser.FileContext context = parser.file();

        stage = Stage.IDENTIFIER_COLLECTION;
        FFile res = new FFile(file);
        SyntaxTreeData treeData = GlobalIdentifierCollector.getIdentifiers(context, res);
        {
            StringBuilder sb = new StringBuilder().append("parsed identifiers:\n");
            res.summary(sb);
            Log.info(this, sb.toString());
        }

        stage = Stage.TO_INTERNAL_REPRESENTATION;
        List<NeedsTypeCheck> typeChecks = ToInternalRepresentation.toInternal(treeData, res);
        {
            StringBuilder sb = new StringBuilder().append("parsed classes:\n");
            res.toString(sb);
            Log.info(this, sb.toString());
        }

        stage = Stage.TYPE_CHECKS;
        NeedsTypeCheck.checkAll(typeChecks);
        Log.info(this, "typecheck passed");

        stage = Stage.FINISHED;
        return res;
    }

    @Override
    public String toString() {
        return "Parser{file='" + file + ", " + stage + '}';
    }
}

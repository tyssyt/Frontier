package tys.frontier.parser;

import com.google.common.collect.BiMap;
import com.opensymphony.xwork2.util.ClassLoaderUtil;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import tys.frontier.code.FFile;
import tys.frontier.code.statement.NeedsTypeCheck;
import tys.frontier.logging.Log;
import tys.frontier.parser.syntaxTree.GlobalIdentifierCollector;
import tys.frontier.parser.syntaxTree.SyntaxTreeData;
import tys.frontier.parser.syntaxTree.ToInternalRepresentation;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxTree.syntaxErrors.SyntaxError;
import tys.frontier.style.StyleOptions;
import tys.frontier.style.StyleUtilities;
import tys.frontier.util.Pair;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

public class Test {

    private static final String FILE_LOCATION = "test.front";


    //TODO this has to move, this is a test
    @org.junit.Test
    public void testParser() {
        //read config
        Pair<StyleOptions, BiMap<String, Integer>> style = StyleUtilities.getStyleFileFromOrDefault(StyleUtilities.DEFAULT_LOCATION, true);

        //create Lexer & Parser & parse
        FrontierLexer lexer;
        try {
            InputStream input = ClassLoaderUtil.getResourceAsStream(FILE_LOCATION, this.getClass());
            lexer = new FrontierLexer(CharStreams.fromStream(input), style.b);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        FrontierParser parser = new FrontierParser(tokens);
        FrontierParser.FileContext context = parser.file();

        FFile file = new FFile(FILE_LOCATION);
        Pair<SyntaxTreeData, List<SyntaxError>> pair = GlobalIdentifierCollector.getIdentifiers(file, context);
        SyntaxTreeData treeData = pair.a;
        List<SyntaxError> errors = pair.b;
        {
            StringBuilder sb = new StringBuilder().append("parsed identifiers:\n");
            file.summary(sb);
            Log.info(this, sb.toString());
        }
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        if (errors.size() > 0) {
            StringBuilder sb = new StringBuilder().append("identifier parser errors: \n");
            errors.forEach(e -> sb.append(e).append('\n'));
            Log.error(this, sb.toString());
            return;
        }

        Pair<List<NeedsTypeCheck>, List<SyntaxError>> typeChecksAndErrors = ToInternalRepresentation.toInternal(file, treeData);
        errors = typeChecksAndErrors.b;
        {
            StringBuilder sb = new StringBuilder().append("parsed classes:\n");
            file.toString(sb);
            Log.info(this, sb.toString());
        }
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        if (errors.size() > 0) {
            StringBuilder sb = new StringBuilder().append("parser errors: \n");
            errors.forEach(e -> sb.append(e).append('\n'));
            Log.error(this, sb.toString());
            return;
        }

        errors.clear();
        for (NeedsTypeCheck n : typeChecksAndErrors.a) {
            try {
                n.checkTypes();
            } catch (IncompatibleTypes e) {
                errors.add(e);
            }
        }
        if (errors.size() > 0) {
            StringBuilder sb = new StringBuilder().append("typecheck errors: \n");
            errors.forEach(e -> sb.append(e).append('\n'));
            Log.error(this, sb.toString());
            return;
        } else {
            Log.info(this, "typecheck passed");
        }
    }

    @Override
    public String toString() {
        return "Test";
    }
}

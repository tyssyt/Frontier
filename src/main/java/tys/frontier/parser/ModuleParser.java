package tys.frontier.parser;

import tys.frontier.code.module.FrontierModule;
import tys.frontier.code.module.Include;
import tys.frontier.parser.syntaxErrors.CyclicInclude;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.style.Style;
import tys.frontier.util.Pair;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.List;
import java.util.Queue;

public class ModuleParser {

    public static FrontierModule buildModule(Path entryPoint, Style style) throws IOException, SyntaxErrors, CyclicInclude {
        FrontierModule res = new FrontierModule();
        ParsedFile entryFile = FileParser.runAntlr(entryPoint, style);
        entryFile.setModule(res);
        Queue<ParsedFile> toDo = new ArrayDeque<>();
        toDo.add(entryFile);

        while (!toDo.isEmpty()) {
            ParsedFile cur = toDo.remove();
            Pair<List<Include>, List<Include>> pair = cur.findIncludes();
            for (Include include : pair.a) {
                if (isCyclicInclude(include.path, cur))
                    throw new CyclicInclude(include.path);

                ParsedFile parsedFile = FileParser.runAntlr(include.path, style);
                parsedFile.setParent(cur);
                cur.addInclude(parsedFile);
                toDo.add(parsedFile);
            }
            res.addNativeIncludes(pair.b);
        }

        res.setEntryPoint(entryFile);
        return res;
    }

    private static boolean isCyclicInclude(Path include, ParsedFile includer) {
        ParsedFile cur = includer;
        while (cur != null) {
            if (cur.getFilePath().equals(include))
                return true;
            cur = cur.getParent();
        }
        return false;
    }




}

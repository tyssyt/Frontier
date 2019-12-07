package tys.frontier.parser;

import tys.frontier.code.module.Module;
import tys.frontier.parser.syntaxErrors.CyclicInclude;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.style.Style;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.Queue;

public class ModuleParser {

    public static Module buildModule(Path entryPoint, Style style) throws IOException, SyntaxErrors, CyclicInclude {
        Module res = new Module();
        ParsedFile entryFile = FileParser.runAntlr(entryPoint, style);
        entryFile.setModule(res);
        Queue<ParsedFile> toDo = new ArrayDeque<>();
        toDo.add(entryFile);

        while (!toDo.isEmpty()) {
            ParsedFile cur = toDo.remove();
            for (Path include : cur.findIncludes()) {
                if (isCyclicInclude(include, cur))
                    throw new CyclicInclude(include);

                ParsedFile parsedFile = FileParser.runAntlr(include, style);
                parsedFile.setParent(cur);
                cur.addInclude(parsedFile);
                toDo.add(parsedFile);
            }
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

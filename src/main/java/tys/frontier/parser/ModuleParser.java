package tys.frontier.parser;

import tys.frontier.code.module.Module;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.style.Style;
import tys.frontier.util.Utils;

import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Queue;

public class ModuleParser {

    public static Module buildModule(String entryPoint, Style style) throws IOException, SyntaxErrors {
        Module res = new Module();
        ParsedFile entryFile = FileParser.runAntlr(entryPoint, style);
        entryFile.setModule(res);
        Queue<ParsedFile> toDo = new ArrayDeque<>();
        toDo.add(entryFile);

        while (!toDo.isEmpty()) {
            ParsedFile cur = toDo.remove();
            for (String include : cur.findIncludes()) {
                if (isCyclicInclude(include, cur))
                    return Utils.NYI("Error for cyclic include");
                ParsedFile parsedFile = FileParser.runAntlr(include, style);
                parsedFile.setParent(cur);
                cur.addInclude(parsedFile);
                toDo.add(parsedFile);
            }
        }

        res.setEntryPoint(entryFile);
        return res;
    }

    private static boolean isCyclicInclude(String include, ParsedFile includer) {
        ParsedFile cur = includer;
        while (cur != null) {
            if (cur.getFileName().equals(include))
                return true;
            cur = cur.getParent();
        }
        return false;
    }




}

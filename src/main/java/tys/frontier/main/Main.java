package tys.frontier.main;

import tys.frontier.backend.llvm.LLVMBackend;
import tys.frontier.code.FFile;
import tys.frontier.parser.Parser;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.style.Style;

import java.io.IOException;

public class Main {

    public static final String in = "D:/Frontier/test/asd.front";
    public static final String out = "D:/Frontier/test/asd.ll";

    public static void main(String[] args) {
        try {
            FFile file = new Parser(in, Style.DEFAULT_STYLE).parse();
            LLVMBackend.runBackend(file, out);
        } catch (IOException | SyntaxErrors e) {
            e.printStackTrace();
        }
    }

}

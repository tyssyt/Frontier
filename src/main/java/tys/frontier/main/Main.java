package tys.frontier.main;

import tys.frontier.backend.llvm.LLVMBackend;
import tys.frontier.code.FFile;
import tys.frontier.code.module.FrontierModule;
import tys.frontier.parser.Parser;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.passes.lowering.FForEachLowering;
import tys.frontier.style.Style;

import java.io.IOException;

public class Main {

    public static final String in = "D:/Frontier/test/asd.front";
    public static final String out = "D:/Frontier/test/asd";
    private static final LLVMBackend.OutputFileType outputType =
            LLVMBackend.OutputFileType.EXECUTABLE;

    public static void main(String[] args) {
        try {
            //FrontEnd
            FrontierModule module = new Parser(in, Style.DEFAULT_STYLE).parse();

            //Lowering Passes
            for (FFile file : module.getFiles()) {
                FForEachLowering.lower(file);
            }

            //Backend
            LLVMBackend.runBackend(module, out, outputType);
        } catch (IOException | SyntaxErrors e) {
            e.printStackTrace();
        }
    }

}

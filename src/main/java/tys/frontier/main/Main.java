package tys.frontier.main;

import tys.frontier.backend.llvm.LLVMBackend;
import tys.frontier.code.FFile;
import tys.frontier.code.module.FrontierModule;
import tys.frontier.parser.Parser;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.passes.lowering.FForEachLowering;
import tys.frontier.passes.lowering.OperatorAssignmentLowering;
import tys.frontier.style.Style;

import java.io.IOException;

public class Main {

    private static final LLVMBackend.OutputFileType outputType =
            LLVMBackend.OutputFileType.EXECUTABLE;

    public static void main(String[] args) {
        try {
            String input = args[0];
            String output = args.length >= 2 ? args[1] : input.substring(0, input.lastIndexOf('.'));

            //FrontEnd
            FrontierModule module = new Parser(input, Style.DEFAULT_STYLE).parse();

            //Lowering Passes
            for (FFile file : module.getFiles()) {
                FForEachLowering.lower(file);
                OperatorAssignmentLowering.lower(file);
            }

            //Backend
            LLVMBackend.runBackend(module, output, outputType);
        } catch (IOException | SyntaxErrors e) {
            e.printStackTrace();
        }
    }

}

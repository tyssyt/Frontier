package tys.frontier.tools;

import org.bytedeco.llvm.LLVM.LLVMModuleRef;
import tys.frontier.backend.llvm.LLVMUtil;
import tys.frontier.util.Utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static com.google.common.io.MoreFiles.getFileExtension;
import static java.util.stream.Collectors.toList;
import static org.bytedeco.llvm.global.LLVM.*;

public class BitcodeLinker {

    private static final String FOLDER = "C:\\Frontier\\imgui\\examples\\example_win32_directx12";
    private static final String OUT = "imgui.bc";

    //fuck compiling llvm-link on windows, why can't people just deliver binaries
    //it is literally less work to load and link the modules with the Java Api then to build that monstrosity...
    public static void main(String[] args) throws IOException {
        List<Path> paths = Files.list(Path.of(FOLDER))
                .filter(path -> getFileExtension(path).equalsIgnoreCase("bc"))
                .collect(toList());

        LLVMModuleRef module = null;
        for (Path path : paths) {
            System.out.println("loading: " + path);
            LLVMModuleRef newModule = LLVMUtil.loadModuleFromBitcode(LLVMGetGlobalContext(), path.toString());

            if (module == null) {
                module = newModule;
                continue;
            }

            int errorCode = LLVMLinkModules2(module, newModule);
            if (errorCode != 0)
                Utils.handleError("failed to Link: " + path + " into the module");
        }
        System.out.println("finished Module");

        int errorId = LLVMWriteBitcodeToFile(module, OUT);
        if (errorId != 0) {
            Utils.handleError("could not write file");
        }
    }

}

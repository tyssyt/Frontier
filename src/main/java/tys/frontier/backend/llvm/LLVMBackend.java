package tys.frontier.backend.llvm;

import tys.frontier.backend.Backend;
import tys.frontier.code.FFile;

import java.util.Collection;
import java.util.Collections;

import static org.bytedeco.javacpp.LLVM.*;

public class LLVMBackend implements Backend {

    public  enum OutputFileType { //TODO this will change, see the todo in tofile of Module
        LLVM_IR("ll"),
        LLVM_BITCODE("bc"),
        TEXTUAL_ASSEMBLY("s"), //TODO from here on content is taget dependend
        NATIVE_OBJECT("o"),
        EXECUTABLE("exe"); //TODO here even the extension is target dependend

        public final String fileExtension;

        OutputFileType(String fileExtension) {
            this.fileExtension = fileExtension;
        }
    }

    //TODO see if LTO is anything worth investing time into

    private static boolean initialized = false;

    synchronized static void initialize() {
        if (initialized)
            return;
        initialized = true;

        LLVMInitializeAllTargetInfos();
        LLVMInitializeAllTargets();
        LLVMInitializeAllTargetMCs();
        LLVMInitializeAllAsmParsers();
        LLVMInitializeAllAsmPrinters();
    }

    public static void runBackend(FFile file, String out, OutputFileType fileType) {
        //TODO a pass that transformes for each into for
        //TODO a pass that creates init function from all field initializers and appends it to constructors
        //TODO optimization oppertunity, when a param is never written to (or dereferenced) we don't have to alloca it... but that can be done by opt passes...
        try (LLVMModule module = createModule(file)) {
            if (out.indexOf('.') == -1)
                out = out + '.' + fileType.fileExtension;
            if (fileType == OutputFileType.LLVM_IR) {
                module.emitToFile(fileType, out);
                return;
            }
            System.out.println(module.emitToString());
            module.optimize(3);
            module.emitToFile(fileType, out);
        }
    }

    public static LLVMModule createModule(FFile file) {
        return createModule(Collections.singleton(file), file.getName());
    }

    public static LLVMModule createModule(Collection<FFile> files, String name) {
        LLVMModule res = new LLVMModule(name);
        for (FFile file : files)
            res.parseTypes(file);
        for (FFile file : files)
            res.parseClassMembers(file);
        res.fillInBodies();
        return res;
    }

}

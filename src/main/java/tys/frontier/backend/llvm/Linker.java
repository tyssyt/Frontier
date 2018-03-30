package tys.frontier.backend.llvm;

import com.google.common.collect.ImmutableList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Linker {
    /*
    For now we hardcode the solution that works exactly only on this machine

    Possible Options for the furure are:
        1. let clang do it <- this
        2. look up how clang does it and copy it
        3. manually try to find a working link and then manually find the needed libs (hell naw)
        4. look into llvm-config https://llvm.org/docs/CommandGuide/llvm-config.html
    */

    public static final String LLVM_Link = "lld-link";
    public static final String MSVC_Link = "\"D:/Programme/Visual Studio/VC/bin/amd64/link.exe\"";

    public static final ImmutableList<String> libs = ImmutableList.<String>builder()
            .add("D:/Programme/Visual Studio/VC/bin/amd64")
            .add("C:/Program Files (x86)/Windows Kits/10/Lib/10.0.10240.0/ucrt/x64")
            .add("C:/Program Files (x86)/Windows Kits/8.1/Lib/winv6.3/um/x64")
            .build();

    public static ProcessBuilder buildCallOld(String fileName, String outName) {
        List<String> inp = new ArrayList<>();
        inp.add(MSVC_Link);
        //inp.add(LLVM_Link);
        //inp.add("-nologo");
        inp.add("-defaultlib:libcmt");
        for (String lib : libs) {
            inp.add("\"-libpath:" + lib + '\"');
        }
        inp.add("-out:" + outName);
        inp.add(fileName);
        return new ProcessBuilder(inp);
    }

    public static ProcessBuilder buildCall(String fileName, String outName) {
        String[] command = new String[]{"clang", "-o", outName, fileName};
        return new ProcessBuilder(Arrays.asList(command)); //TODO maybe this can actually be called from the clang api?
    }
}

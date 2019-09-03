package tys.frontier.backend.llvm;

import com.google.common.collect.ImmutableList;
import tys.frontier.util.OS;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Linker {

    public static ProcessBuilder buildCall(String inputFile, String putputFile) {
        if (OS.isWindows()) {
            return buildCallWindows(inputFile, putputFile);
        } else {
            return buildCallClang(inputFile, putputFile); //TODO linux linker call
        }
    }

    public static ProcessBuilder buildCallClang(String inputFile, String outputFile) {
        String[] command = new String[]{"clang", "-o", outputFile, inputFile};
        return new ProcessBuilder(Arrays.asList(command)); //TODO maybe this can actually be called from the clang api?
    }

    public static ProcessBuilder buildCallWindows(String inputFile, String outputFile) {
        //TODO make this work when running as jar
        String linker = Linker.class.getResource("lld-link.exe").getFile();
        List<String> libDirs = new ArrayList<>();
        libDirs.add(Linker.class.getResource("lib/x64/").getFile().substring(1)); //TODO un-hard-code //getResource adds a slash at the beginning

        ImmutableList.Builder<String> builder = ImmutableList.builder();
        builder.add(linker).add("-nologo").add("-defaultlib:libcmt").add("-out:" + outputFile);
        for (String libDir : libDirs) {
            builder.add("-libpath:" + libDir);
        }
        builder.add(inputFile);
        return new ProcessBuilder(builder.build());
    }
}

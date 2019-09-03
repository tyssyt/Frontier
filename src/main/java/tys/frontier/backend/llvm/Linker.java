package tys.frontier.backend.llvm;

import com.google.common.collect.ImmutableList;
import tys.frontier.util.JarUtils;
import tys.frontier.util.OS;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Linker {

    public static ProcessBuilder buildCall(String inputFile, String outputFile, String targetTriple) {
        if (OS.isWindows()) {
            return buildCallWindows(inputFile, outputFile, targetTriple);
        } else {
            return buildCallClang(inputFile, outputFile); //TODO linux linker call
        }
    }

    public static ProcessBuilder buildCallClang(String inputFile, String outputFile) {
        String[] command = new String[]{"clang", "-o", outputFile, inputFile};
        return new ProcessBuilder(Arrays.asList(command)); //TODO maybe this can actually be called from the clang api?
    }

    public static ProcessBuilder buildCallWindows(String inputFile, String outputFile, String targetTriple) {
        if (JarUtils.isRunningInJar()) {
            return Utils.NYI("running in Jar"); //TODO
        }

        String linker = Linker.class.getResource("lld-link.exe").getFile();
        List<String> libDirs = new ArrayList<>();
        String pathToLib = "lib/" + getArch(targetTriple) + '/';
        libDirs.add(Linker.class.getResource(pathToLib).getFile().substring(1)); //getResource adds a slash at the beginning

        ImmutableList.Builder<String> builder = ImmutableList.builder();
        builder.add(linker).add("-nologo").add("-defaultlib:libcmt").add("-out:" + outputFile);
        for (String libDir : libDirs) {
            builder.add("-libpath:" + libDir);
        }
        builder.add(inputFile);
        return new ProcessBuilder(builder.build());
    }

    private static String getArch(String targetTriple) { //TODO if we ever do a class for TT, move this there
        String llvmArch = targetTriple.substring(0, targetTriple.indexOf('-'));
        switch (llvmArch) {
            case "x86":
                return "x86";
            case "x86_64":
                return "x64";
            case "arm":
                return "arm";
            case "aarch64":
                return "arm64";
            default:
                return Utils.handleError("unknown Architecture: " + llvmArch);
        }
    }
}

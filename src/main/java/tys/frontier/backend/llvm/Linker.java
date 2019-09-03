package tys.frontier.backend.llvm;

import com.google.common.collect.ImmutableList;
import tys.frontier.util.JarUtils;
import tys.frontier.util.OS;
import tys.frontier.util.Utils;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.jar.JarFile;

public class Linker { //TODO clean up seperators

    public static ProcessBuilder buildCall(String inputFile, String outputFile, List<String> libDirs, String targetTriple) {
        if (OS.isWindows()) {
            return buildCallWindows(inputFile, outputFile, libDirs, targetTriple);
        } else {
            return buildCallClang(inputFile, outputFile); //TODO linux linker call
        }
    }

    public static ProcessBuilder buildCallClang(String inputFile, String outputFile) {
        String[] command = new String[]{"clang", "-o", outputFile, inputFile};
        return new ProcessBuilder(Arrays.asList(command)); //TODO maybe this can actually be called from the clang api?
    }

    public static ProcessBuilder buildCallWindows(String inputFile, String outputFile, List<String> libDirs, String targetTriple) {
        String linker;
        if (JarUtils.isRunningInJar()) {
            //we must unpack the linker and the libraries from the jar

            //check if the folder already exist
            final File runningJar = JarUtils.getRunningJar();
            String folderName = runningJar.getName();
            folderName = folderName.substring(0, folderName.length() - 4); //remove .jar
            final File targetFolder = new File(runningJar.getParent() + '/' + folderName);
            if (!targetFolder.exists()) {
                try {
                    copyLinkerFromJar(targetFolder); //unpack if they don't
                } catch (IOException e) {
                    return Utils.handleException(e);
                }
            }

            //get linker & libs dir
            linker = targetFolder.getPath() + "/lld-link.exe";
            libDirs.add(targetFolder.getPath() + "/lib/" + getArch(targetTriple) + '/');
        } else {
            linker = Linker.class.getResource("lld-link.exe").getFile();
            String pathToLib = "lib/" + getArch(targetTriple) + '/';
            libDirs.add(Linker.class.getResource(pathToLib).getFile().substring(1)); //getResource adds a slash at the beginning
        }


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

    private static void copyLinkerFromJar(File targetFolder) throws IOException {
        Path path = targetFolder.toPath();
        //copy the lib folder
        JarUtils.copyFolderFromJar(new JarFile(JarUtils.getRunningJar()), "tys/frontier/backend/llvm/lib", path.resolve("lib"));
        //copy the linker
        Files.copy(Linker.class.getResourceAsStream("lld-link.exe"), path.resolve("lld-link.exe"));
    }
}

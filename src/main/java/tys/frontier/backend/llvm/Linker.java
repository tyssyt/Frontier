package tys.frontier.backend.llvm;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.module.Include;
import tys.frontier.util.JarUtils;
import tys.frontier.util.OS;
import tys.frontier.util.Utils;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.JarFile;

import static java.util.Arrays.asList;

public class Linker { //TODO clean up seperators

    public static ProcessBuilder buildCall(String inputFile, String outputFile, List<Include> userLibs, String targetTriple) {
        List<String> inputFiles = new ArrayList<>();
        List<String> weirdExcludeThingies = new ArrayList<>();
        inputFiles.add(inputFile);

        //add all Libs as inputFiles
        for (Include userLib : userLibs) {
            inputFiles.add(userLib.path.toString());
            if (userLib.out)
                weirdExcludeThingies.add(userLib.path.getFileName().toString());
        }

        if (OS.isWindows()) {
            return buildCallWindows(inputFiles, outputFile, weirdExcludeThingies, targetTriple);
        } else {
            return buildCallClang(inputFile, outputFile); //TODO linux linker call
        }
    }

    public static ProcessBuilder buildCallClang(String inputFile, String outputFile) {
        String[] command = new String[]{"clang", "-o", outputFile, inputFile};
        return new ProcessBuilder(asList(command)); //TODO maybe this can actually be called from the clang api?
    }

    public static ProcessBuilder buildCallWindows(List<String> inputFiles, String outputFile, List<String> weirdExcludeThingies, String targetTriple) {
        String linker;
        String libDir;
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
            libDir = targetFolder.getPath() + "/lib/" + getArch(targetTriple) + '/';
        } else {
            linker = Linker.class.getResource("lld-link.exe").getFile();
            String pathToLib = "lib/" + getArch(targetTriple) + '/';
            libDir = Linker.class.getResource(pathToLib).getFile().substring(1); //getResource adds a slash at the beginning
        }


        ImmutableList.Builder<String> builder = ImmutableList.builder();
        builder.add(linker).add("-nologo").add("-defaultlib:libcmt").add("-out:" + outputFile);
        builder.add("-libpath:" + libDir);
        builder.addAll(inputFiles);
        builder.add("-nodefaultlib:OLDNAMES.lib");
        for (String weirdExcludeThingy : weirdExcludeThingies)
            builder.add("-nodefaultlib:" + weirdExcludeThingy);
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

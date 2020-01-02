package tys.frontier.util;

import com.google.common.io.Resources;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class FileUtils {

    private FileUtils() {}

    public static boolean deleteDir(File file) {
        File[] contents = file.listFiles();
        if (contents != null) {
            for (File f : contents) {
                if (!Files.isSymbolicLink(f.toPath())) {
                    deleteDir(f);
                }
            }
        }
        return file.delete();
    }

    public static Path pathToResource(String resource) {
        try {
            return Paths.get(Resources.getResource(resource).toURI());
        } catch (URISyntaxException e) {
            return null;
        }
    }

}

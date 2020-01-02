package tys.frontier.util;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

public class JarUtils {

    private JarUtils () {}

    private static final File pathToRunningJar = getPathToRunningJar();

    public static boolean isRunningInJar() {
        return pathToRunningJar != null;
    }

    public static File getRunningJar() {
        return pathToRunningJar;
    }

    public static void copyFolderFromJar(JarFile jar, String nameInJar, Path targetFolder) throws IOException {
        for (Enumeration<JarEntry> entries = jar.entries(); entries.hasMoreElements(); ) {
            JarEntry entry = entries.nextElement();
            if (entry.getName().startsWith(nameInJar + "/") && !entry.isDirectory()) {
                String subPath = entry.getName().substring(nameInJar.length() + 1);
                Path dest = targetFolder.resolve(subPath);

                File parent = dest.getParent().toFile();
                if (parent != null) {
                    //noinspection ResultOfMethodCallIgnored
                    parent.mkdirs();
                }
                Files.copy(jar.getInputStream(entry), dest);
            }
        }
    }

    private static File getPathToRunningJar() {
        try {
            return getPathToRunningJarFromProtectionDomain();
        } catch (SecurityException | NullPointerException | URISyntaxException ignored) {}
        try {
            return getPathToRunningJarFromResource();
        } catch (URISyntaxException e) {
            return Utils.handleException(e);
        }
    }

    private static File getPathToRunningJarFromProtectionDomain() throws SecurityException, NullPointerException, URISyntaxException {
        String codeSourceLocation = JarUtils.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath();
        File file = new File(codeSourceLocation);
        if (file.isDirectory())
            return null; //not running in jar
        else
            return file;
    }

    private static File getPathToRunningJarFromResource() throws URISyntaxException {
        String className = '/' + JarUtils.class.getName().replace('.', '/') + ".class";
        String url = JarUtils.class.getResource(className).toURI().toString();
        assert (url.endsWith(className));

        if (!url.startsWith("jar:"))
            return null;

        // remove the "jar:file:" prefix and "!/className" suffix
        String path = url.substring(9, url.length() - (className.length() + 1));
        return new File(path);
    }

}

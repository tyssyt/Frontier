package tys.frontier.util;

public class OS {

    public static final String OS = System.getProperty("os.name");
    private static final String os = OS.toLowerCase();

    private OS () {}

    public static boolean isWindows() {
        return os.startsWith("windows");
    }

}

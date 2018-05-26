package tys.frontier.util;

import com.opensymphony.xwork2.util.ClassLoaderUtil;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.identifier.FVariableIdentifier;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public final class Utils {
    private Utils() {}

    public static void handleException (Exception e) {
        throw new RuntimeException(e);
    }

    public static void handleError(String s) {
        throw new RuntimeException(s);
    }

    public static <T> T NYI(String s) {
        throw new RuntimeException(s + "not yet implemented. ¯\\_(ツ)_/¯");
    }

    public static <T> T cantHappen() {
        throw new RuntimeException("¯\\_(ツ)_/¯");
    }

    public static Map<FVariableIdentifier, FLocalVariable> asMap (Collection<? extends FLocalVariable> vars) {
        Map<FVariableIdentifier, FLocalVariable> map = new HashMap<>();
        for (FLocalVariable v : vars) {
            if (map.put(v.getIdentifier(), v) != null) {
                throw new IllegalStateException("Duplicate key");
            }
        }
        return map;
    }

    public static InputStream loadFile(String file) throws FileNotFoundException {
        InputStream input = ClassLoaderUtil.getResourceAsStream(file, Utils.class);
        if (input == null)
            input = new FileInputStream(file);
        return input;
    }
}

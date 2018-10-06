package tys.frontier.util;

import com.opensymphony.xwork2.util.ClassLoaderUtil;
import tys.frontier.code.FClass;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FVariableIdentifier;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.*;

public final class Utils {
    private Utils() {}

    public static void handleException (Exception e) {
        throw new RuntimeException(e);
    }

    public static void handleError(String s) {
        throw new RuntimeException(s);
    }

    public static <T> T NYI(String s) {
        throw new RuntimeException(s + " not yet implemented. ¯\\_(ツ)_/¯");
    }

    public static <T> T cantHappen() {
        throw new RuntimeException("¯\\_(ツ)_/¯");
    }

    public static String removeLeadingUnderscores(String in) {
        int i=0;
        while (i<in.length()) {
            if (in.charAt(i) != '_')
                break;
            i++;
        }
        return in.substring(i);
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

    public static List<FClass> typesFromExpressionList(List<FExpression> exps) {
        List<FClass> res = new ArrayList<>(exps.size());
        for (FExpression exp : exps)
            res.add(exp.getType());
        return res;
    }
}

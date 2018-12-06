package tys.frontier.util;

import com.google.errorprone.annotations.CanIgnoreReturnValue;
import com.opensymphony.xwork2.util.ClassLoaderUtil;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FType;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.*;

public final class Utils {
    private Utils() {}

    public static <T> T handleException (Exception e) {
        throw new RuntimeException(e);
    }

    public static <T> T handleError(String s) {
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

    public static <T extends IdentifierNameable> Map<FIdentifier, T> asMap (Collection<? extends T> vars) {
        Map<FIdentifier, T> map = new HashMap<>();
        for (T t : vars) {
            if (map.put(t.getIdentifier(), t) != null) {
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

    public static List<FType> typesFromExpressionList(List<FExpression> exps) {
        List<FType> res = new ArrayList<>(exps.size());
        for (FExpression exp : exps)
            res.add(exp.getType());
        return res;
    }

    public static FField getFieldInClass(FField field, FClass in) {
        if (field.getMemberOf() == in)
            return field;
        if (field.isInstance())
            return in.getInstanceFields().get(field.getIdentifier());
        else
            return in.getStaticFields().get(field.getIdentifier());
    }

    public static FFunction getFunctionInClass(FFunction function, FClass in) {
        if (function.getMemberOf() == in)
            return function;
        for (FFunction f : in.getFunctions().get(function.getIdentifier()))
            if (f.getSignature().equals(function.getSignature()))
                return f;
        return null;
    }

    @CanIgnoreReturnValue
    public static StringBuilder joinIdentifiers(StringBuilder sb, Iterator<? extends IdentifierNameable> nameables, String separator) {
        if (nameables.hasNext()) {
            sb.append(nameables.next().getIdentifier());
            while (nameables.hasNext()) {
                sb.append(separator);
                sb.append(nameables.next().getIdentifier());
            }
        }
        return sb;
    }

    @CanIgnoreReturnValue
    public static StringBuilder joinIdentifiers(StringBuilder sb, Iterable<? extends IdentifierNameable> nameables, String separator) {
        return joinIdentifiers(sb, nameables.iterator(), separator);
    }
}

package tys.frontier.util;

import tys.frontier.code.FVariable;
import tys.frontier.code.identifier.FVariableIdentifier;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public final class Utils {
    private Utils() {}

    public static Map<FVariableIdentifier, FVariable> asMap (Collection<FVariable> vars) {
        Map<FVariableIdentifier, FVariable> map = new HashMap<>();
        for (FVariable v : vars) {
            if (map.put(v.getIdentifier(), v) != null) {
                throw new IllegalStateException("Duplicate key");
            }
        }
        return map;
    }
}

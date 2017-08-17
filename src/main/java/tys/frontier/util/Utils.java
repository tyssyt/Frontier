package tys.frontier.util;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.identifier.FVariableIdentifier;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public final class Utils {
    private Utils() {}

    public static Map<FVariableIdentifier, FLocalVariable> asMap (Collection<FLocalVariable> vars) {
        Map<FVariableIdentifier, FLocalVariable> map = new HashMap<>();
        for (FLocalVariable v : vars) {
            if (map.put(v.getIdentifier(), v) != null) {
                throw new IllegalStateException("Duplicate key");
            }
        }
        return map;
    }
}

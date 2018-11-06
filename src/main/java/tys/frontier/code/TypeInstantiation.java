package tys.frontier.code;

import tys.frontier.code.predefinedClasses.FInstantiatedClass;
import tys.frontier.util.Utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TypeInstantiation {

    public static final TypeInstantiation EMPTY = new TypeInstantiation(null) {
        @Override
        public boolean isEmpty() {
            return true;
        }
        @Override
        public FType getType(FType original) {
            return original;
        }
        @Override
        public TypeInstantiation intersect(List<FTypeVariable> typeVariables) {
            return this;
        }
        @Override
        public TypeInstantiation then(TypeInstantiation other) {
            return other;
        }
        @Override
        public boolean equals(Object o) {
            return o instanceof TypeInstantiation && ((TypeInstantiation) o).isEmpty();
        }
        @Override
        public int hashCode() {
            return 0;
        }
    };

    private Map<FTypeVariable, FType> typeMap;

    public TypeInstantiation(Map<FTypeVariable, FType> typeMap) {
        this.typeMap = typeMap;
    }

    public boolean isEmpty() {
        return typeMap.isEmpty();
    }

    public FType getType(FType original) {
        if (original instanceof FClass) {
            return FInstantiatedClass.from((FClass) original, this);
        } else if (original instanceof FTypeVariable) {
            return typeMap.getOrDefault(original, original);
        } else {
            return original;
        }
    }

    public TypeInstantiation intersect (List<FTypeVariable> typeVariables) {
        Map<FTypeVariable, FType> newMap = new HashMap<>(typeMap);
        boolean changed = newMap.keySet().retainAll(typeVariables);
        if (changed)
            return new TypeInstantiation(newMap);
        else
            return this;
    }

    public TypeInstantiation then(TypeInstantiation other) {
        if (other.isEmpty())
            return this;
        return Utils.NYI("combining instantiations");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TypeInstantiation)) return false;

        TypeInstantiation that = (TypeInstantiation) o;

        return typeMap.equals(that.typeMap);
    }

    @Override
    public int hashCode() {
        return typeMap.hashCode();
    }
}

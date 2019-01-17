package tys.frontier.code;

import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.util.Utils;

import java.util.*;

public class TypeInstantiation {

    public static final TypeInstantiation EMPTY = new TypeInstantiation(Collections.emptyMap()) {
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
            return this == o ||
                (o instanceof TypeInstantiation && ((TypeInstantiation) o).isEmpty());
        }
        @Override
        public int hashCode() {
            return 0;
        }
    };

    private Map<FTypeVariable, FType> typeMap;

    private TypeInstantiation(Map<FTypeVariable, FType> typeMap) {
        this.typeMap = typeMap;
    }

    public Map<FTypeVariable, FType> getTypeMap() {
        return typeMap;
    }

    public static TypeInstantiation create(Map<FTypeVariable, FType> typeMap) {
        if (typeMap.isEmpty())
            return EMPTY;
        return new TypeInstantiation(typeMap);
    }

    public boolean isEmpty() {
        return typeMap.isEmpty();
    }

    public boolean contains(FTypeVariable var) {
        return typeMap.containsKey(var);
    }

    public <T extends HasTypeParameters<T>> boolean fits(HasTypeParameters<T> fClass) {
        return typeMap.size() == fClass.getParameters().size() && typeMap.keySet().containsAll(fClass.getParameters().values());
    }

    public FType getType(FType original) {
        if (original instanceof FArray) {
            FArray array = (FArray) original;
            return FArray.getArrayFrom(getType(array.getBaseType()));
        } else if (original instanceof FOptional) {
                FOptional optional = (FOptional) original;
                return FOptional.fromFlatten(getType(optional.getBaseType()));
        } else if (original instanceof FFunctionType) {
            FFunctionType fFunctionType = (FFunctionType) original;
            List<FType> in = new ArrayList<>(fFunctionType.getIn().size());
            for (FType fType : fFunctionType.getIn()) {
                in.add(getType(fType));
            }
            return FFunctionType.from(in, getType(fFunctionType.getOut()));
        } else if (original instanceof FClass) {
            return ((FClass) original).getInstantiation(this);
        } else if (original instanceof FTypeVariable) {
            return typeMap.getOrDefault(original, original);
        } else {
            return Utils.cantHappen();
        }
    }

    public TypeInstantiation intersect (List<FTypeVariable> typeVariables) {
        Map<FTypeVariable, FType> newMap = new HashMap<>(typeMap);
        boolean changed = newMap.keySet().retainAll(typeVariables);
        if (changed)
            return create(newMap);
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

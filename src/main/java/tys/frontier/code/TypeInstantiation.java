package tys.frontier.code;

import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.HasCall;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.TypeConstraint;
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

    /**
     * Do not modify the map after calling this!
     */
    public static TypeInstantiation create(Map<FTypeVariable, FType> typeMap) {
        if (typeMap.isEmpty())
            return EMPTY;
        //clean up the map to make sure no lhs appears on a rhs
        for (Map.Entry<FTypeVariable, FType> entry : typeMap.entrySet()) {
            entry.setValue(resolveTrans(entry.getValue(), typeMap));
        }
        return new TypeInstantiation(typeMap);
    }

    private static FType resolveTrans(FType startType, Map<FTypeVariable, FType> typeMap) {
        FType type = startType;
        while (true) {
            if (!(type instanceof FTypeVariable)) {
                return type;
            }

            FType _new = typeMap.get(type);
            if (_new == null) {
                return type;
            }
            type = _new;
            if (startType == _new) //cycle detection
                return Utils.handleError("types were resolved cyclic"); //TODO
        }
    }

    public boolean isEmpty() {
        return typeMap.isEmpty();
    }

    public boolean contains(FTypeVariable var) {
        return typeMap.containsKey(var);
    }

    public <T extends HasTypeParameters<T>> boolean fits(T hasParam) {
        return typeMap.size() == hasParam.getParameters().size() && typeMap.keySet().containsAll(hasParam.getParameters().values());
    }

    public boolean fitsIgnoreReturn(FFunction function) {
        if (fits(function))
            return true;
        FType returnType = function.getType();
        if (returnType instanceof FTypeVariable && function.getParameters().get(returnType.getIdentifier()) == returnType) {
            //the return type is a parameter
            if (typeMap.size() != function.getParameters().size()-1)
                return false;
            for (FTypeVariable param : function.getParametersList()) {
                if (param != returnType && !typeMap.containsKey(param))
                    return false;
            }
            return true;
        }
        return false;
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

    @SuppressWarnings("unchecked")
    public <C extends TypeConstraint> C getConstraint(C constraint) {
        if (constraint instanceof ImplicitCastable) {
            ImplicitCastable implicitCastable = (ImplicitCastable) constraint;
            FType newType = getType(implicitCastable.getTarget());
            if (newType == implicitCastable.getTarget())
                return constraint;
            else
                return (C) new ImplicitCastable(constraint.getOrigin(), newType, implicitCastable.getVariance());
        } else if (constraint instanceof HasCall) {
            return constraint;
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

    public boolean disjoint(TypeInstantiation other) {
        return Utils.disjoint(this.typeMap.keySet(), other.typeMap.keySet());
    }

    public TypeInstantiation then(TypeInstantiation other) {
        if (other.isEmpty())
            return this;
        Map<FTypeVariable, FType> newMap = new HashMap<>(other.typeMap);
        for (Map.Entry<FTypeVariable, FType> entry : this.typeMap.entrySet()) {
            FType old = newMap.put(entry.getKey(), other.getType(entry.getValue()));
            assert old == null;
        }
        return create(newMap);
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

package tys.frontier.code;

import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.namespace.TypeVariableNamespace.ReturnTypeOf;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.NonEmbeddableType;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.util.Utils;

import java.util.*;

public class TypeInstantiation {

    public static final TypeInstantiation EMPTY = new TypeInstantiation(Collections.emptyMap()) {
        @Override
        public TypeInstantiation intersect(Collection<FTypeVariable> typeVariables) {
            return this;
        }
        @Override
        public TypeInstantiation with(TypeInstantiation other) {
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
        @Override
        public FType getType(FType original) {
            return original;
        }
    };

    private Map<FTypeVariable, FType> typeMap;

    private TypeInstantiation(Map<FTypeVariable, FType> typeMap) {
        this.typeMap = typeMap;
    }

    /**
     * Do not modify the map after calling this!
     */
    public static TypeInstantiation create(Map<FTypeVariable, FType> typeMap) {
        if (typeMap.isEmpty())
            return EMPTY;
        TypeInstantiation typeInstantiation = new TypeInstantiation(typeMap);
        typeInstantiation.clean();
        return typeInstantiation;
    }

    private void clean() {
        typeMap.replaceAll((k, v) -> getType(v));
    }

    public boolean isEmpty() {
        return typeMap.isEmpty();
    }

    public boolean contains(FTypeVariable var) {
        return typeMap.containsKey(var);
    }

    public Set<Map.Entry<FTypeVariable, FType>> entries() {
        return typeMap.entrySet();
    }

    public Set<FTypeVariable> keys() {
        return typeMap.keySet();
    }

    public Collection<FType> values() {
        return typeMap.values();
    }


    public boolean fits(FFunction hasParam) {
        return typeMap.size() == hasParam.getParameters().size() && typeMap.keySet().containsAll(hasParam.getParameters().values());
    }

    public FType getType(FType original) {
        assert original != null;
        if (original instanceof FOptional) { //TODO can this be removed when Optionals use TypeVaribles?
                FOptional optional = (FOptional) original;
                return FOptional.fromFlatten(getType(optional.getBaseType()));
        } else if (original instanceof FFunctionType) { //TODO can this be removed when Functions use TypeVaribles?
            FFunctionType fFunctionType = (FFunctionType) original;
            return FFunctionType.from(getType(fFunctionType.getIn()), getType(fFunctionType.getOut()));
        } else if (original instanceof FTuple) {
            FTuple fTuple = (FTuple) original;
            List<FType> newTypes = new ArrayList<>(fTuple.arity());
            for (FType type : fTuple.getTypes()) {
                newTypes.add(getType(type));
            }
            return FTuple.from(newTypes);
        } else if (original instanceof FClass) {
            FClass fClass = (FClass) original;
            List<FType> args = new ArrayList<>(fClass.getParametersList().size());
            for (FType fType : fClass.getParametersList()) {
                args.add(getType(fType));
            }
            try {
                return fClass.getInstantiation(args);
            } catch (WrongNumberOfTypeArguments | NonEmbeddableType syntaxError) {
                return Utils.cantHappen();
            }
        } else if (original instanceof FTypeVariable) {
            FTypeVariable typeVariable = (FTypeVariable) original;

            FType res = typeMap.get(original);
            if (res != null)
                return res;

            if (typeVariable instanceof ReturnTypeOf) {
                ReturnTypeOf returnTypeOf = (ReturnTypeOf) typeVariable;
                FType oldMemberOf = returnTypeOf.getBase();
                if (oldMemberOf != null) {
                    FType newMemberOf = getType(oldMemberOf);

                    if (newMemberOf != oldMemberOf)
                        return instantiatedReturnType(returnTypeOf, newMemberOf.getNamespace());
                }
            }

            return typeVariable;
        } else {
            return Utils.cantHappen();
        }
    }

    private FType instantiatedReturnType(ReturnTypeOf returnTypeOf, Namespace newMemberOf) {
        List<FType> positionalArgs = Utils.map(returnTypeOf.getPositionalArgs(), this::getType);
        Map<FIdentifier, FType> keywordArgs = Utils.map(returnTypeOf.getKeywordArgs(), this::getType);
        try {
            Signature instantiation = newMemberOf.resolveFunction(returnTypeOf.getFunction().getIdentifier(),
                    positionalArgs, keywordArgs, null, returnTypeOf.isLhsResolve(), returnTypeOf.getUnbounds()
                ).signature;
            //if (!(instantiation.getType() instanceof FTypeVariable.ReturnTypeOf))
            return getType(instantiation.getType());
        } catch (FunctionNotFound functionNotFound) {
            return Utils.handleException(functionNotFound);
        }
    }

    public TypeInstantiation intersect (Collection<FTypeVariable> typeVariables) {
        Map<FTypeVariable, FType> newMap = new HashMap<>(typeMap);
        boolean changed = newMap.keySet().retainAll(typeVariables);
        if (changed)
            return create(newMap);
        else
            return this;
    }

    public TypeInstantiation with(TypeInstantiation other) {
        if (other.isEmpty())
            return this;
        Map<FTypeVariable, FType> newMap = new HashMap<>(this.typeMap);
        newMap.putAll(other.typeMap);
        return create(newMap);
    }

    public TypeInstantiation with(FTypeVariable from, FType to) {
        Map<FTypeVariable, FType> newMap = new HashMap<>(this.typeMap);
        newMap.put(from, to);
        return create(newMap);
    }

    @Override
    public String toString() {
        return typeMap.toString();
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

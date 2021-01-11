package tys.frontier.code;

import com.google.common.collect.ListMultimap;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.namespace.TypeVariableNamespace.IterationElementType;
import tys.frontier.code.namespace.TypeVariableNamespace.ReturnTypeOf;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.HasCall;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.util.Utils;

import java.util.*;

import static java.util.Collections.singletonMap;

public class TypeInstantiation {

    public static final TypeInstantiation EMPTY = new TypeInstantiation(Collections.emptyMap()) {
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

    /**
     * Do not modify the map after calling this!
     */
    public static TypeInstantiation create(Map<FTypeVariable, FType> typeMap) {
        if (typeMap.isEmpty())
            return EMPTY;
        //clean up the map to make sure no lhs appears on a rhs
        Iterator<Map.Entry<FTypeVariable, FType>> it = typeMap.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<FTypeVariable, FType> entry = it.next();
            if (entry.getKey() == entry.getValue())
                it.remove();
            else
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

    public Set<Map.Entry<FTypeVariable, FType>> entries() {
        return typeMap.entrySet();
    }

    public Collection<FType> values() {
        return typeMap.values();
    }


    public boolean fits(FFunction hasParam) {
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
            } catch (WrongNumberOfTypeArguments wrongNumberOfTypeArguments) {
                return Utils.cantHappen();
            }
        } else if (original instanceof FTypeVariable) {
            FTypeVariable typeVariable = (FTypeVariable) original;
            if (typeVariable.isResolved())
                return getType(typeVariable.getResolved());

            FType res = typeMap.get(original);
            if (res != null)
                return getType(res);

            if (original instanceof IterationElementType) {
                IterationElementType iterationElementType = (IterationElementType) original;
                FTypeVariable oldIterable = iterationElementType.getBase();
                FType newIterable = getType(oldIterable);

                if (newIterable != oldIterable)
                    return getType(newIterable.getForImpl().getElementType());
            }

            if (original instanceof ReturnTypeOf) {
                ReturnTypeOf returnTypeOf = (ReturnTypeOf) original;
                FType oldMemberOf = returnTypeOf.getBase();
                if (oldMemberOf != null) {
                    FType newMemberOf = getType(oldMemberOf);

                    if (newMemberOf != oldMemberOf)
                        return instantiatedReturnType(returnTypeOf, newMemberOf.getNamespace());
                }
            }

            return original;
        } else {
            return Utils.cantHappen();
        }
    }

    private FType instantiatedReturnType(ReturnTypeOf returnTypeOf, Namespace newMemberOf) {
        List<FType> positionalArgs = Utils.map(returnTypeOf.getPositionalArgs(), this::getType);
        ListMultimap<FIdentifier, FType> keywordArgs = Utils.map(returnTypeOf.getKeywordArgs(), this::getType);
        try {
            Signature instantiation = newMemberOf.hardResolveFunction(returnTypeOf.getFunction().getIdentifier(),
                    positionalArgs, keywordArgs, null, returnTypeOf.isLhsResolve()).signature;
            //if (!(instantiation.getType() instanceof FTypeVariable.ReturnTypeOf))
            return getType(instantiation.getType());
        } catch (FunctionNotFound functionNotFound) {
            return Utils.handleException(functionNotFound);
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

    public TypeInstantiation with(FTypeVariable from, FType to) {
        return new TypeInstantiation(singletonMap(from, to)).then(this);
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

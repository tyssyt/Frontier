package tys.frontier.util;

import com.google.common.collect.*;
import com.google.errorprone.annotations.CanIgnoreReturnValue;
import tys.frontier.code.FField;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.Typed;
import tys.frontier.code.function.ClassInstantiationFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.*;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.syntaxErrors.FieldNotFound;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;

import java.util.*;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

public final class Utils {

    public static final String endl = System.getProperty("line.separator");
    public static final String filesep = System.getProperty("file.separator");

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

    public static <T extends FType> Map<FTypeIdentifier, T> asTypeMap (Collection<? extends T> vars) {
        Map<FTypeIdentifier, T> map = new HashMap<>();
        for (T t : vars) {
            if (map.put(t.getIdentifier(), t) != null) {
                throw new IllegalStateException("Duplicate key");
            }
        }
        return map;
    }

    public static <S,T> ArrayList<T> map(Collection<S> source, Function<S, T> mapper) {
        ArrayList<T> res = new ArrayList<>(source.size());
        for (S s : source)
            res.add(mapper.apply(s));
        return res;
    }

    public static <K,S,T> HashMap<K,T> map(Map<K,S> source, Function<S, T> mapper) {
        HashMap<K,T> res = new HashMap<>();
        for (Map.Entry<K, S> entry : source.entrySet())
            res.put(entry.getKey(), mapper.apply(entry.getValue()));
        return res;
    }

    public static <K,S,T> ListMultimap<K,T> map(Multimap<K,S> source, Function<S, T> mapper) {
        ListMultimap<K,T> res = MultimapBuilder.hashKeys().arrayListValues().build();
        for (Map.Entry<K, S> entry : source.entries())
            res.put(entry.getKey(), mapper.apply(entry.getValue()));
        return res;
    }

    public static List<FType> typesFromExpressionList(List<? extends Typed> exps) {
        return Lists.transform(exps, Typed::getType);
    }

    public static <T> ListMultimap<T, FType> typesFromExpressionMap(ListMultimap<T, ? extends Typed> exps) {
        return Multimaps.transformValues(exps, Typed::getType);
    }

    public static List<FType> typesFromExpressionList(List<? extends Typed> exps, UnaryOperator<FType> op) {
        List<FType> res = new ArrayList<>(exps.size());
        for (Typed exp : exps)
            res.add(op.apply(exp.getType()));
        return res;
    }

    public static FField findFieldInstantiation(FField field, TypeInstantiation typeInstantiation) {
        FType namespace = typeInstantiation.getType(field.getMemberOf());
        try {
            return namespace.getField(field.getIdentifier());
        } catch (FieldNotFound fieldNotFound) {
            return cantHappen();
        }
    }

    public static FFunction findFunctionInstantiation(FFunction function, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, TypeInstantiation typeInstantiation) {
        //handle namespace/class instantiation
        FType oldNamespace = function.getMemberOf();
        FType newNamespace = typeInstantiation.getType(oldNamespace);

        if (newNamespace instanceof FTypeVariable)
            return Utils.cantHappen();

        if (oldNamespace instanceof FTypeVariable) {
            //there is no mapping we can follow, we need to fall back to use resolve
            FFunctionIdentifier identifier = function.getIdentifier();
            if (identifier instanceof FInstantiatedFunctionIdentifier)
                identifier = ((FInstantiatedFunctionIdentifier) identifier).baseIdentifier;
            else
                identifier = function.getIdentifier();

            FType returnType = typeInstantiation.getType(function.getType());

            try {
                return newNamespace.hardResolveFunction(identifier, positionalArgs, keywordArgs, returnType).function;
            } catch (FunctionNotFound functionNotFound) {
                return Utils.cantHappen();
            }
        }

        if (oldNamespace != newNamespace) {
            //TODO oh god pls make arrys, optionals and function types use generics!
            if (oldNamespace instanceof FArray) {
                //arrays only have the constructor
                assert function.isConstructor();
                return ((FArray) newNamespace).getConstructor();
            } else if (oldNamespace instanceof FOptional) {
                return Utils.NYI("instantiation lookup for optionals"); //TODO
            } else if (oldNamespace instanceof FFunctionType) {
                return Utils.cantHappen(); //for now function types have no callable functions
            }

            if (oldNamespace instanceof FInstantiatedClass) {
                //if the old namespace is also an instantiation, go back to the base
                FInstantiatedClass instantiatedClass = (FInstantiatedClass) oldNamespace;
                function = ((ClassInstantiationFunction) function).getProxy();
                oldNamespace = instantiatedClass.getProxy();
            }
            //now go to the new instantiation
            assert newNamespace instanceof FInstantiatedClass;
            FInstantiatedClass instantiatedClass = (FInstantiatedClass) newNamespace;
            assert instantiatedClass.getProxy() == oldNamespace;
            function = instantiatedClass.getInstantiatedFunction(function);
        }

        //handle function instantiation
        return function.getInstantiation(typeInstantiation);
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

    public static <T> boolean disjoint(Set<T> a, Set<T> b) {
        if (a.size() < b.size()) {
            for (T t : a) {
                if (b.contains(t))
                    return false;
            }
        } else {
            for (T t : b) {
                if (a.contains(t))
                    return false;
            }
        }
        return true;
    }

    public static  <T> T firstDuplicate(Set<T> a, Set<T> b) {
        if (a.size() < b.size()) {
            for (T t : a) {
                if (b.contains(t))
                    return t;
            }
        } else {
            for (T t : b) {
                if (a.contains(t))
                    return t;
            }
        }
        return null;
    }

    public static <T> int countNonNull(Iterable<T> iterable) {
        int res=0;
        for (T t : iterable)
            if (t != null)
                res++;
        return res;
    }

    public static Map<FFunction, String> computeUniqueFunctionNames(ListMultimap<FFunctionIdentifier, FFunction> functions) {
        Map<FFunction, String> res = new HashMap<>();
        for (List<FFunction> list : Multimaps.asMap(functions).values()) {
            String name = list.get(0).getIdentifier().name;

            if (list.size() == 1) {
                res.put(list.get(0), name);
                continue;
            }

            //TODO when multithreading is used we might need to copy the list first before sorting to avoid race conditions while sorting
            list.sort((f1, f2) -> {
                int c = f1.getParams().size() - f2.getParams().size();
                if (c != 0)
                    return c;
                for (int i=0; i<f1.getParams().size(); i++) {
                    String id1 = f1.getParams().get(i).getType().getIdentifier().name;
                    String id2 = f2.getParams().get(i).getType().getIdentifier().name;
                    c = id1.compareTo(id2);
                    if (c != 0)
                        return c;
                }
                return 0;
            });
            for (int i=0; i<list.size(); i++) {
                res.put(list.get(i), name + "#" + i);
            }
        }
        return res;
    }

    public static <T, S extends T> Stream<S> filterCast(Stream<T> stream, Class<S> target) {
        return stream.filter(target::isInstance).map(target::cast);
    }

    public static <T,S> Iterator<Pair<T,S>> zip(Iterator<T> it1, Iterator<S> it2) {
        return new Iterator<Pair<T, S>>() {
            @Override
            public boolean hasNext() {
                return it1.hasNext() && it2.hasNext() ;
            }

            @Override
            public Pair<T, S> next() {
                return new Pair<>(it1.next(), it2.next());
            }

            @Override
            public void remove() {
                it1.remove();
                it2.remove();
            }
        };
    }

    public static <T,S> Iterable<Pair<T,S>> zip(Iterable<T> it1, Iterable<S> it2) {
        return () -> zip(it1.iterator(), it2.iterator());
    }

}

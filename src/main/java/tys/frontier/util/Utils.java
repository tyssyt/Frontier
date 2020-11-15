package tys.frontier.util;

import com.google.common.collect.*;
import com.google.errorprone.annotations.CanIgnoreReturnValue;
import tys.frontier.code.FField;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.Typed;
import tys.frontier.code.function.*;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FInstantiatedFunctionIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
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

    @SuppressWarnings("ArraysAsListWithZeroOrOneArgument") //TODO custom implementation
    public static <T> List<T> mutableSingletonList(T item) {
        return Arrays.asList(item);
    }

    public static <K, V> Map<K, V> mutableSingletonMap(K key, V value) { //TODO custom implementation
        HashMap<K, V> map = new HashMap<>(1, 1);
        map.put(key, value);
        return map;
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

    public static <T extends FType> Map<FIdentifier, T> asTypeMap (Collection<? extends T> vars) {
        Map<FIdentifier, T> map = new HashMap<>();
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

    public static <S,T> ImmutableList<T> mapImmutable(Collection<S> source, Function<S, T> mapper) {
        ImmutableList.Builder<T> builder = ImmutableList.builder();
        for (S s : source)
            builder.add(mapper.apply(s));
        return builder.build();
    }

    public static <S,T> HashSet<T> map(Set<S> source, Function<S, T> mapper) {
        HashSet<T> res = new HashSet<>(source.size());
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

    private static boolean needsReResolve(FFunction function, Namespace namespace, FIdentifier identifier) {
        if (function instanceof DummyFunction)
            return true;
        if (namespace.getOpen(identifier) != null) //TODO optimisation: only reresolve when the original call needed casts
            return true;
        return false;
    }

    public static Signature findFunctionInstantiation(Signature signature, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, TypeInstantiation typeInstantiation) {
        //handle namespace/class instantiation
        FFunction function = signature.getFunction();
        Namespace oldNamespace = function.getMemberOf();
        FType oldType = oldNamespace.getType();

        FIdentifier identifier = function.getIdentifier();
        if (identifier instanceof FInstantiatedFunctionIdentifier)
            identifier = ((FInstantiatedFunctionIdentifier) identifier).baseIdentifier;
        else
            identifier = function.getIdentifier();
        if (needsReResolve(function, oldNamespace, identifier)) {
            FType returnType = typeInstantiation.getType(signature.getType());

            Namespace namespace;
            if (oldType instanceof FTypeVariable) {
                namespace = typeInstantiation.getType(oldType).getNamespace();
            } else {
                namespace = oldNamespace;
            }

            try {
                return namespace.hardResolveFunction(identifier, positionalArgs, keywordArgs, returnType, signature.isLhs()).signature;
            } catch (FunctionNotFound functionNotFound) {
                return Utils.cantHappen();
            }
        }

        if (oldType == null)
            return signature;
        FType newType = typeInstantiation.getType(oldType);
        Namespace newNamespace = newType.getNamespace();

        assert !(oldType instanceof FTypeVariable) && !(newType instanceof FTypeVariable);

        if (oldNamespace != newNamespace) {
            //TODO oh god pls make arrys, optionals and function types use generics!
            if (oldType instanceof FArray) {
                assert newType instanceof FArray;
                if (function.isConstructor()) {
                    return ((FArray) newType).getConstructor().getSignature();
                } else if (function.getIdentifier().equals(Access.ID)) {
                    for (Signature s : ((DefaultNamespace) newNamespace).getFunctions(signature.isLhs()).get(Access.ID)) { //TODO not required if assignee functions can't appear on rhs!
                        if (s.getParameters().size() == signature.getParameters().size())
                            return s;
                    }
                    return Utils.cantHappen();
                } else if (function instanceof FieldAccessor) {
                    FieldAccessor accessor = (FieldAccessor) function;
                    assert accessor.getField().isInstance() && accessor.getField().getIdentifier().equals(FArray.SIZE);
                    FieldAccessor inst = getAccessor((FArray) newType, FArray.SIZE, true, accessor.isGetter());
                    return signature.isLhs() ? inst.getLhsSignature() : inst.getSignature();
                } else {
                    return Utils.cantHappen();
                }
            } else if (oldType instanceof FOptional) {
                assert newType instanceof FOptional;
                if (identifier.equals(FOptional.EXMARK)) {
                    FFunction inst = ((FOptional) newType).getExmark();
                    return signature.isLhs() ? inst.getLhsSignature() : inst.getSignature();
                }
                return Utils.NYI("instantiation lookup for optionals"); //TODO
            } else if (oldType instanceof FFunctionType) {
                return Utils.cantHappen(); //for now function types have no callable functions
            }

            if (oldType instanceof FInstantiatedClass) {
                //if the old namespace is also an instantiation, go back to the base
                FInstantiatedClass instantiatedClass = (FInstantiatedClass) oldType;
                oldType = instantiatedClass.getProxy();
                oldNamespace = oldType.getNamespace();
                if (function instanceof ClassInstantiationFunction)
                    function = ((ClassInstantiationFunction) function).getProxy();
                else if (function instanceof FieldAccessor)
                    function = getAccessor((FClass) oldType, (FieldAccessor) function);
                else
                    return Utils.cantHappen();
            }
            //now go to the new instantiation
            assert newType instanceof FInstantiatedClass;
            FInstantiatedClass instantiatedClass = (FInstantiatedClass) newType;
            assert instantiatedClass.getProxy() == oldType;
            function = instantiatedClass.getInstantiatedFunction(function);
        }

        //handle function instantiation

        FFunction instantiation = function.getInstantiation(typeInstantiation);
        return signature.isLhs() ? instantiation.getLhsSignature() : instantiation.getSignature();
    }

    public static FieldAccessor getAccessor(FClass in, FieldAccessor like) {
        return getAccessor(in, like.getIdentifier(), like.isInstance(), like.isGetter());
    }

    public static FieldAccessor getAccessor(FClass in, FIdentifier identifier, boolean isInstance, boolean isGetter) {
        BiMap<FIdentifier, FField> fields = isInstance ? in.getInstanceFields() : in.getStaticFields();
        FField field = fields.get(identifier);
        return isGetter ? field.getGetter() : field.getSetter();
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

    public static <T> T firstDuplicate(Set<T> a, Set<T> b) {
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

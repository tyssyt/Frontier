package tys.frontier.util;

import com.google.errorprone.annotations.CanIgnoreReturnValue;
import com.opensymphony.xwork2.util.ClassLoaderUtil;
import tys.frontier.code.*;
import tys.frontier.code.function.ClassInstantiationFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FInstantiatedFunctionIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.parser.syntaxErrors.FieldNotFound;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.*;
import java.util.function.UnaryOperator;

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

    public static List<FType> typesFromExpressionList(List<? extends Typed> exps) {
        List<FType> res = new ArrayList<>(exps.size());
        for (Typed exp : exps)
            res.add(exp.getType());
        return res;
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

    public static FFunction findFunctionInstantiation(FFunction function, List<FType> argumentTypes, TypeInstantiation typeInstantiation) {
        //handle namespace/class instantiation
        FType oldNamespace = function.getMemberOf();
        FType newNamespace = typeInstantiation.getType(oldNamespace);

        if (newNamespace instanceof FTypeVariable ||oldNamespace instanceof FTypeVariable) {
            //there is no mapping we can follow, we need to fall back to use resolve
            FFunctionIdentifier identifier = function.getIdentifier();
            if (identifier instanceof FInstantiatedFunctionIdentifier)
                identifier = ((FInstantiatedFunctionIdentifier) identifier).baseIdentifier;
            else
                identifier = function.getIdentifier();
            try {
                FFunction res = newNamespace.resolveFunction(identifier, argumentTypes, TypeInstantiation.EMPTY);

                if (res.getType() instanceof FTypeVariable && typeInstantiation.getTypeMap().containsKey((FTypeVariable)res.getType())) {
                    /*
                        There is a special problematic case:
                        When the return type is a TypeVariable, that does not appear anywhere else in the header
                        resolve can't properly instantiate it and will leave it as is without constraints.
                        However, our typeInstantiation can have a mapping for the return type and thus res is not fully instantiated yet.
                        This usually only appears with lambdas.

                        Possible solutions include instantiating the already instatiated function again (weakening our instantiation contract).
                        Or allowing us to pass a type for the return into resolve, making resolve even more complex.
                     */
                    return Utils.NYI("correctly instantiating the return type of " + res.headerToString() + " to " + typeInstantiation.getType(res.getType()));
                }
                return res;
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
                oldNamespace = instantiatedClass.getBaseClass();
            }
            //now go to the new instantiation
            assert newNamespace instanceof FInstantiatedClass;
            FInstantiatedClass instantiatedClass = (FInstantiatedClass) newNamespace;
            assert instantiatedClass.getBaseClass() == oldNamespace;
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
}

package tys.frontier.passes.analysis.reachability;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.FField;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.expression.FFunctionAddress;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FInstantiatedFunction;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.FClassVisitor;
import tys.frontier.parser.syntaxErrors.FieldNotFound;
import tys.frontier.util.Triple;
import tys.frontier.util.Utils;

import java.util.*;

public class Reachability {

    public static class ReachableClass {
        public final Set<FField> reachableFields = new HashSet<>();
        public final Multimap<FFunction, FInstantiatedFunction> reachableFunctions = ArrayListMultimap.create();
    }

    private Map<FClass, ReachableClass> reachableClasses = new HashMap<>();

    private Reachability() {}

    public static Reachability analyse (Set<FFunction> startingPoints) {
        //TODO sanity check on startingPoints, making sure they are not generic (i.e. instance functions of a generic class)
        Reachability res = new Reachability();
        Deque<FFunction> todoFunctions = new ArrayDeque<>(startingPoints);
        Deque<FField> todoFields = new ArrayDeque<>();
        FClassVisitor reachabilityVisitor = reachabilityVisitor(todoFunctions, todoFields);

        while (!todoFunctions.isEmpty() || !todoFields.isEmpty()) {
            if (!todoFunctions.isEmpty()) {
                FFunction cur = todoFunctions.pollFirst();
                if (res.isReachable(cur))
                    continue;

                res.addFunction(cur);

                if (cur.getMemberOf() instanceof FOptional) { //TODO if we ever switch to optional handling in front end, this is no longer needed
                    todoFunctions.addFirst(((FOptional) cur.getMemberOf()).getOriginalFunction(cur));
                    continue;
                }

                if (cur.isInstantiation()) { //for instantiated functions, we have to visit the base instead because they are not yet baked
                    Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>> baseAnalysis = analyseBase(cur.getBaseR());

                    handleBaseAnalysis(baseAnalysis, cur.getTypeInstantiationToBase(), todoFunctions, todoFields);
                } else { //normal function
                    cur.accept(reachabilityVisitor);
                }
            } else {
                FField cur = todoFields.pollFirst();
                if (res.isReachable(cur))
                    continue;

                res.addField(cur);

                //TODO when fields work in optionals, we need similar handling of optionals here as for functions above

                if (cur.getMemberOf() instanceof FInstantiatedClass) {//for fields in instantiated classes, we have to visit the base instead because they are not yet baked
                    FInstantiatedClass instantiatedClass = (FInstantiatedClass) cur.getMemberOf();
                    FField base;
                    try {
                        base = instantiatedClass.getProxy().getField(cur.getIdentifier());
                    } catch (FieldNotFound fieldNotFound) {
                        return Utils.cantHappen();
                    }
                    Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>> baseAnalysis = analyseBase(base);
                    handleBaseAnalysis(baseAnalysis, instantiatedClass.getTypeInstantiation(), todoFunctions, todoFields);
                } else { //normal field
                    cur.accept(reachabilityVisitor);
                }
            }
        } //end while
        return res;
    }

    private static FClassVisitor reachabilityVisitor(Collection<FFunction> seenFunctions, Collection<FField> seenFields) {
        return new FClassVisitor() {
            @Override
            public void enterFieldAccess(FFieldAccess fieldAccess) {
                seenFields.add(fieldAccess.getField());
            }

            @Override
            public void enterFunctionCall(FFunctionCall functionCall) {
                seenFunctions.add(functionCall.getFunction());
            }

            @Override
            public FExpression visitFunctionAddress(FFunctionAddress address) {
                seenFunctions.add(address.getFunction());
                return address;
            }
        };
    }
    private static FClassVisitor reachabilityVisitor(Set<FFunctionCall> seenCalls, Set<FFieldAccess> seenFields, Set<FFunctionAddress> seenAddresses) {
        return new FClassVisitor() {
            @Override
            public void enterFieldAccess(FFieldAccess fieldAccess) {
                seenFields.add(fieldAccess);
            }
            @Override
            public void enterFunctionCall(FFunctionCall functionCall) {
                seenCalls.add(functionCall);
            }
            @Override
            public FExpression visitFunctionAddress(FFunctionAddress address) {
                seenAddresses.add(address);
                return address;
            }
        };
    }

    private static Map<FFunction, Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>>> cachedBaseFunctionAnalysis = new HashMap<>();
    private static Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>> analyseBase(FFunction base) {
        Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>> res = cachedBaseFunctionAnalysis.get(base);
        if (res == null) {
            res = new Triple<>(new HashSet<>(), new HashSet<>(), new HashSet<>());
            base.accept(reachabilityVisitor(res.a, res.b, res.c));
            cachedBaseFunctionAnalysis.put(base, res);
        }
        return res;
    }

    private static Map<FField, Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>>> cachedBaseFieldAnalysis = new HashMap<>();
    private static Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>> analyseBase(FField base) {
        Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>> res = cachedBaseFieldAnalysis.get(base);
        if (res == null) {
            res = new Triple<>(new HashSet<>(), new HashSet<>(), new HashSet<>());
            base.accept(reachabilityVisitor(res.a, res.b, res.c));
            cachedBaseFieldAnalysis.put(base, res);
        }
        return res;
    }

    private static void handleBaseAnalysis(Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>> baseAnalysis, TypeInstantiation typeInstantiation, Collection<FFunction> seenFunctions, Collection<FField> seenFields) {
        //Instantiate Base results and add to Q/reach
        for (FFunctionCall fC : baseAnalysis.a) {
            List<FType> paramTypes = Utils.typesFromExpressionList(fC.getArguments(), typeInstantiation::getType);
            FFunction f = Utils.findFunctionInstantiation(fC.getFunction(), paramTypes, Collections.emptyMap(), typeInstantiation);
            seenFunctions.add(f);
        }
        for (FFieldAccess fA : baseAnalysis.b) {
            FField f = Utils.findFieldInstantiation(fA.getField(), typeInstantiation);
            seenFields.add(f);
        }
        for (FFunctionAddress fA : baseAnalysis.c) {
            List<FType> paramTypes = Utils.typesFromExpressionList(fA.getFunction().getParams(), typeInstantiation::getType);
            FFunction f = Utils.findFunctionInstantiation(fA.getFunction(), paramTypes, Collections.emptyMap(), typeInstantiation);
            seenFunctions.add(f);
        }
    }

    private void addFunction(FFunction function) {
        ReachableClass reachableClass = reachableClasses.computeIfAbsent((FClass) function.getMemberOf(), x -> new ReachableClass());
        FInstantiatedFunction value = null;
        if (function instanceof FInstantiatedFunction) {
            FInstantiatedFunction fInstantiatedFunction = (FInstantiatedFunction) function;
            value = fInstantiatedFunction;
            function = fInstantiatedFunction.getProxy();
        }
        reachableClass.reachableFunctions.put(function, value);
    }

    private void addField(FField field) {
        ReachableClass reachableClass = reachableClasses.computeIfAbsent(field.getMemberOf(), x -> new ReachableClass());
        reachableClass.reachableFields.add(field);
    }

    public boolean isReachable(FClass _class) {
        return reachableClasses.containsKey(_class);
    }

    public boolean isReachable(FField field) {
        ReachableClass reachableClass = reachableClasses.get(field.getMemberOf());
        return reachableClass != null && reachableClass.reachableFields.contains(field);
    }

    @SuppressWarnings("SuspiciousMethodCalls")
    public boolean isReachable(FFunction function) {
        ReachableClass reachableClass = reachableClasses.get(function.getMemberOf());
        if (reachableClass == null)
            return false;
        if (function instanceof FInstantiatedFunction)
            return reachableClass.reachableFunctions.get(((FInstantiatedFunction) function).getProxy()).contains(function);
        else
            return reachableClass.reachableFunctions.containsKey(function);
    }

    public Map<FClass, ReachableClass> getReachableClasses() {
        return reachableClasses;
    }
}

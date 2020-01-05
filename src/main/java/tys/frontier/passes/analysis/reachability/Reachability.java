package tys.frontier.passes.analysis.reachability;

import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.MultimapBuilder;
import com.google.common.collect.SetMultimap;
import tys.frontier.code.FField;
import tys.frontier.code.FTypeMember;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionAddress;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FInstantiatedFunction;
import tys.frontier.code.function.FieldAccessor;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.FClassVisitor;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;

public class Reachability {

    public static class ReachableClass {
        public final SetMultimap<FFunction, FInstantiatedFunction> reachableFunctions = MultimapBuilder.hashKeys().hashSetValues().build();

        public boolean isReachable (FFunction function) {
            if (function instanceof FInstantiatedFunction)
                return reachableFunctions.get(((FInstantiatedFunction) function).getProxy()).contains(function);
            else
                return reachableFunctions.containsKey(function);
        }

        public boolean isReachable (FField field) {
            return isReachable(field.getGetter()) || isReachable(field.getSetter());
        }
    }

    private Map<FClass, ReachableClass> reachableClasses = new HashMap<>();

    private Reachability() {}

    public static Reachability analyse (Set<FFunction> startingPoints) {
        //TODO sanity check on startingPoints, making sure they are not generic (i.e. instance functions of a generic class)
        Reachability res = new Reachability();
        Deque<FFunction> todoFunctions = new ArrayDeque<>(startingPoints);
        FClassVisitor reachabilityVisitor = reachabilityVisitor(todoFunctions);

        while (!todoFunctions.isEmpty()) {
            FFunction cur = todoFunctions.pollFirst();
            if (res.isReachable(cur))
                continue;

            if (cur instanceof FieldAccessor) {
                //field
                FField field = ((FieldAccessor) cur).getField();
                if (res.isReachable(field))
                    continue;

                res.addFunction(cur);
                //TODO when fields work in optionals, we need similar handling of optionals here as for functions below
                if (field.getMemberOf() instanceof FInstantiatedClass) {//for fields in instantiated classes, we have to visit the base instead because they are not yet baked
                    FInstantiatedClass instantiatedClass = (FInstantiatedClass) field.getMemberOf();
                    FField base;
                    if (field.isInstance()) {
                        base = instantiatedClass.getProxy().getInstanceFields().get(field.getIdentifier());
                    } else {
                        base = instantiatedClass.getProxy().getStaticFields().get(field.getIdentifier());
                    }
                    Pair<Set<FFunctionCall>, Set<FFunctionAddress>> baseAnalysis = analyseBase(base);
                    handleBaseAnalysis(baseAnalysis, instantiatedClass.getTypeInstantiation(), todoFunctions);
                } else { //normal field
                    field.accept(reachabilityVisitor);
                }
            } else {
                res.addFunction(cur);
                //function
                if (cur.getMemberOf() instanceof FOptional) { //TODO if we ever switch to optional handling in front end, this is no longer needed
                    todoFunctions.addFirst(((FOptional) cur.getMemberOf()).getOriginalFunction(cur));
                    continue;
                }

                if (cur.isInstantiation()) { //for instantiated functions, we have to visit the base instead because they are not yet baked
                    Pair<Set<FFunctionCall>, Set<FFunctionAddress>> baseAnalysis = analyseBase(cur.getBaseR());
                    handleBaseAnalysis(baseAnalysis, cur.getTypeInstantiationToBase(), todoFunctions);
                } else { //normal function
                    cur.accept(reachabilityVisitor);
                }
            }
        } //end while
        return res;
    }

    private static FClassVisitor reachabilityVisitor(Collection<FFunction> seenFunctions) {
        return new FClassVisitor() {

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
    private static FClassVisitor reachabilityVisitor(Set<FFunctionCall> seenCalls, Set<FFunctionAddress> seenAddresses) {
        return new FClassVisitor() {
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

    private static Map<FTypeMember, Pair<Set<FFunctionCall>, Set<FFunctionAddress>>> cachedBaseFunctionAnalysis = new HashMap<>();
    private static Pair<Set<FFunctionCall>, Set<FFunctionAddress>> analyseBase(FTypeMember base) {
        Pair<Set<FFunctionCall>, Set<FFunctionAddress>> res = cachedBaseFunctionAnalysis.get(base);
        if (res == null) {
            res = new Pair<>(new HashSet<>(), new HashSet<>());
            if (base instanceof FField) //TODO this should be doable with an interface but Java doesn't agree
                ((FField)base).accept(reachabilityVisitor(res.a, res.b));
            else if (base instanceof FFunction)
                ((FFunction)base).accept(reachabilityVisitor(res.a, res.b));
            else
                return Utils.cantHappen();
            cachedBaseFunctionAnalysis.put(base, res);
        }
        return res;
    }

    private static void handleBaseAnalysis(Pair<Set<FFunctionCall>, Set<FFunctionAddress>> baseAnalysis, TypeInstantiation typeInstantiation, Collection<FFunction> seenFunctions) {
        //Instantiate Base results and add to Q/reach
        for (FFunctionCall fC : baseAnalysis.a) {
            List<FType> paramTypes = Utils.typesFromExpressionList(fC.getArguments(), typeInstantiation::getType);
            FFunction f = Utils.findFunctionInstantiation(fC.getSignature(), paramTypes, ImmutableListMultimap.of(), typeInstantiation).getFunction();
            seenFunctions.add(f);
        }
        for (FFunctionAddress fA : baseAnalysis.b) {
            List<FType> paramTypes = Utils.typesFromExpressionList(fA.getFunction().getSignature().getParameters(), typeInstantiation::getType);
            FFunction f = Utils.findFunctionInstantiation(fA.getFunction().getSignature(), paramTypes, ImmutableListMultimap.of(), typeInstantiation).getFunction();
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

    @SuppressWarnings("SuspiciousMethodCalls")
    public boolean isReachable(FFunction function) {
        ReachableClass reachableClass = reachableClasses.get(function.getMemberOf());
        if (reachableClass == null)
            return false;
        return reachableClass.isReachable(function);
    }

    public boolean isReachable(FField field) {
        ReachableClass reachableClass = reachableClasses.get(field.getMemberOf());
        if (reachableClass == null)
            return false;
        return reachableClass.isReachable(field);
    }

    public Map<FClass, ReachableClass> getReachableClasses() {
        return reachableClasses;
    }
}

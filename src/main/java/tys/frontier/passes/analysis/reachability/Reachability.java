package tys.frontier.passes.analysis.reachability;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.*;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.expression.FFunctionAddress;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.visitor.FClassVisitor;
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
        Deque<FFunction> todo = new ArrayDeque<>(startingPoints);
        while (!todo.isEmpty()) {
            FFunction cur = todo.pollFirst();
            if (res.isReachable(cur))
                continue;

            res.addFunction(cur);

            if (cur.getMemberOf() instanceof FOptional) { //TODO if we ever switch to optional handling in front end, this is no longer needed
                todo.addFirst(((FOptional) cur.getMemberOf()).getOriginalFunction(cur));
                continue;
            }

            if (cur instanceof FInstantiatedFunction) { //for instantiated functions, we have to visit the base instead because they are not yet baked
                FInstantiatedFunction instantiatedFunction = (FInstantiatedFunction) cur;
                TypeInstantiation typeInstantiation = instantiatedFunction.getTypeInstantiation();
                //Analyse Base
                Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>> baseAnalysis = analyseBase(instantiatedFunction.getBase());
                //Instantiate Base results and add to Q/reach
                for (FFunctionCall fC : baseAnalysis.a) {
                    List<FType> paramTypes = Utils.typesFromExpressionList(fC.getArguments(), typeInstantiation::getType);
                    FFunction f = Utils.findFunctionInstantiation(fC.getFunction(), paramTypes, typeInstantiation);
                    todo.addLast(f);
                }
                for (FFieldAccess fA : baseAnalysis.b) {
                    FField f = Utils.findFieldInstantiation(fA.getField(), typeInstantiation);
                    res.addField(f);
                }
                for (FFunctionAddress fA : baseAnalysis.c) {
                    List<FType> paramTypes = Utils.typesFromExpressionList(fA.getFunction().getParams(), typeInstantiation::getType);
                    FFunction f = Utils.findFunctionInstantiation(fA.getFunction(), paramTypes, typeInstantiation);
                    todo.addLast(f);
                }
            } else { //normal function
                cur.accept(new FClassVisitor() {
                    @Override
                    public void enterFieldAccess(FFieldAccess fieldAccess) {
                        res.addField(fieldAccess.getField());
                    }
                    @Override
                    public void enterFunctionCall(FFunctionCall functionCall) {
                        todo.addLast(functionCall.getFunction());
                    }
                    @Override
                    public FExpression visitFunctionAddress(FFunctionAddress address) {
                        todo.addLast(address.getFunction());
                        return address;
                    }
                });
            }
        }
        return res;
    }

    private static Map<FFunction, Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>>> cachedBaseAnalysis = new HashMap<>();
    private static Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>> analyseBase(FFunction base) {
        Triple<Set<FFunctionCall>, Set<FFieldAccess>, Set<FFunctionAddress>> res = cachedBaseAnalysis.get(base);
        if (res == null) {
            Set<FFunctionCall> reachableFunctions = new HashSet<>();
            Set<FFieldAccess> reachableFields = new HashSet<>();
            Set<FFunctionAddress> reachableFunctionAddresses = new HashSet<>();
            res = new Triple<>(reachableFunctions, reachableFields, reachableFunctionAddresses);
            base.accept(new FClassVisitor() {
                @Override
                public void enterFieldAccess(FFieldAccess fieldAccess) {
                    reachableFields.add(fieldAccess);
                }
                @Override
                public void enterFunctionCall(FFunctionCall functionCall) {
                    reachableFunctions.add(functionCall);
                }
                @Override
                public FExpression visitFunctionAddress(FFunctionAddress address) {
                    reachableFunctionAddresses.add(address);
                    return address;
                }
            });
            cachedBaseAnalysis.put(base, res);
        }
        return res;
    }

    private void addFunction(FFunction function) {
        ReachableClass reachableClass = reachableClasses.computeIfAbsent((FClass) function.getMemberOf(), x -> new ReachableClass());
        FInstantiatedFunction value = null;
        if (function instanceof FInstantiatedFunction) {
            FInstantiatedFunction fInstantiatedFunction = (FInstantiatedFunction) function;
            if (fInstantiatedFunction.getInstantiationType() == FInstantiatedFunction.InstantiationType.FUNCTION_INSTANTIATION) {
                value = fInstantiatedFunction;
                function = fInstantiatedFunction.getBase();
            }
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
        if (function instanceof FInstantiatedFunction && ((FInstantiatedFunction) function).getInstantiationType() == FInstantiatedFunction.InstantiationType.FUNCTION_INSTANTIATION)
            return reachableClass.reachableFunctions.get(((FInstantiatedFunction) function).getBase()).contains(function);
        else
            return reachableClass.reachableFunctions.containsKey(function);
    }

    public Map<FClass, ReachableClass> getReachableClasses() {
        return reachableClasses;
    }
}

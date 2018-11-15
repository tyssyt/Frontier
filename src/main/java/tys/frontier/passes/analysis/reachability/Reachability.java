package tys.frontier.passes.analysis.reachability;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.predefinedClasses.FInstantiatedClass;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.visitor.FClassVisitor;

import java.util.*;

public class Reachability {

    public static class ReachableClass {
        public final Set<FField> reachableFields = new HashSet<>();
        public final Set<FFunction> reachableFunctions = new HashSet<>();
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

            if (cur.getMemberOf() instanceof FInstantiatedClass) {
                FInstantiatedClass parent = (FInstantiatedClass) cur.getMemberOf();
                TypeInstantiation typeInstantiation = parent.getTypeInstantiation();

                parent.getOriginalFunction(cur).accept(new FClassVisitor() {
                    @Override
                    public void enterFieldAccess(FFieldAccess fieldAccess) {
                        FField f = fieldAccess.getField();
                        FClass c = f.getMemberOf();
                        c = (FClass) typeInstantiation.getType(c);
                        if (c == parent.getBaseClass())
                            c = parent;

                        ReachableClass reachableClass = res.reachableClasses.computeIfAbsent(c, x -> new ReachableClass());
                        reachableClass.reachableFields.add(f);
                    }
                    @Override
                    public void enterFunctionCall(FFunctionCall functionCall) {
                        FFunction f = functionCall.getFunction();
                        FClass orig = f.getMemberOf();
                        FClass c = (FClass) typeInstantiation.getType(orig); //this cast is safe as reachability analysis only traverses fully instatiated classes
                        if (orig != c) {
                            f = ((FInstantiatedClass) c).getInstantiatedFunction(f); //this cast is safe because either c is an instatiated class or orig a TypeVariable, and Type Variables can't have functions
                        }
                        if (c == parent.getBaseClass()) {
                            f = parent.getInstantiatedFunction(f);
                        }
                        todo.addLast(f);
                    }
                });
            } else
                cur.accept(new FClassVisitor() {
                    @Override
                    public void enterFieldAccess(FFieldAccess fieldAccess) {
                        res.addField(fieldAccess.getField());
                    }
                    @Override
                    public void enterFunctionCall(FFunctionCall functionCall) {
                        todo.addLast(functionCall.getFunction());
                    }
                });
        }
        return res;
    }

    private void addFunction(FFunction function) {
        ReachableClass reachableClass = reachableClasses.computeIfAbsent(function.getMemberOf(), x -> new ReachableClass());
        reachableClass.reachableFunctions.add(function);
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

    public boolean isReachable(FFunction function) {
        ReachableClass reachableClass = reachableClasses.get(function.getMemberOf());
        return reachableClass != null && reachableClass.reachableFunctions.contains(function);
    }

    public Map<FClass, ReachableClass> getReachableClasses() {
        return reachableClasses;
    }
}

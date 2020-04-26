package tys.frontier.passes.analysis.reachability;

import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.MultimapBuilder;
import com.google.common.collect.SetMultimap;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionAddress;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FInstantiatedFunction;
import tys.frontier.code.function.FieldAccessor;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.namespace.OptionalNamespace;
import tys.frontier.code.predefinedClasses.FFieldType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.loop.FForEach;
import tys.frontier.code.statement.loop.forImpl.PrimitiveFor;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.visitor.FClassVisitor;
import tys.frontier.util.Triple;
import tys.frontier.util.Utils;

import java.util.*;

import static com.google.common.collect.Iterables.getOnlyElement;

public class Reachability {

    public static class ReachableNamespace {
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

    private static class BaseAnalysis {
        BaseAnalysis parentSegment;
        List<Triple<BaseAnalysis, FTypeVariable, FTypeVariable>> subSegments = new ArrayList<>(); //special logic for primitive for loops that get lowered during baking
        Set<FFunctionCall> seenCalls = new HashSet<>();
        Set<FFunctionAddress> seenAddresses = new HashSet<>();

        private void handle(TypeInstantiation typeInstantiation, Collection<FFunction> seenFunctions) {
            doHandle(typeInstantiation, seenFunctions);

            for (Triple<BaseAnalysis, FTypeVariable, FTypeVariable> subSegment : subSegments) {
                assert typeInstantiation.contains(subSegment.c);
                FClass instantiatedType = (FClass) typeInstantiation.getType(subSegment.c);

                //TODO special cases like array & tuple, see primitive for lowering
                for (FField field : instantiatedType.getFields()) {
                    subSegment.a.handle(typeInstantiation.with(subSegment.b, field.getType()), seenFunctions);
                }
            }
        }

        private void doHandle(TypeInstantiation typeInstantiation, Collection<FFunction> seenFunctions) {
            //Instantiate Base results and add to Q/reach
            for (FFunctionCall fC : seenCalls) {
                //We can't use fc.getArguments, because default Values have not been baked, so we work around it by finding the base signature and calling the formerly private fillDefaultArgs
                Signature sig = fC.getSignature().isLhs() ? fC.getFunction().getBaseR().getLhsSignature()
                        :  fC.getFunction().getBaseR().getSignature();
                List<FType> paramTypes = Utils.typesFromExpressionList(fC.fillDefaultArgs(sig), typeInstantiation::getType);

                FFunction f = Utils.findFunctionInstantiation(fC.getSignature(), paramTypes, ImmutableListMultimap.of(), typeInstantiation).getFunction();
                seenFunctions.add(f);
            }
            for (FFunctionAddress fA : seenAddresses) {
                List<FType> paramTypes = Utils.typesFromExpressionList(fA.getFunction().getSignature().getParameters(), typeInstantiation::getType);
                FFunction f = Utils.findFunctionInstantiation(fA.getFunction().getSignature(), paramTypes, ImmutableListMultimap.of(), typeInstantiation).getFunction();
                seenFunctions.add(f);
            }
        }
    }

    private Map<DefaultNamespace, ReachableNamespace> reachableNamespaces = new HashMap<>();

    private Reachability() {
        //TODO not strictly necessary, but there are weird cases where one can use types and not fields and that breaks the backend, because removeUnused is disabled on TypeType
        //TODO at the same time there are still cases where the getter get removed and running multiple tests in a row is broken...
        addNamespace(FTypeType.INSTANCE.getNamespace());
        addFunction(FTypeType.allTypes.getGetter());
        addFunction(FTypeType.fields.getGetter());
        addFunction(FTypeType.name.getGetter());
        addNamespace(FFieldType.INSTANCE.getNamespace());
        addFunction(FFieldType.type.getGetter());
        addFunction(FFieldType.name.getGetter());
    }

    public static Reachability analyse (Set<FFunction> startingPoints) {
        //TODO sanity check on startingPoints, making sure they are not generic (i.e. instance functions of a generic class)
        Reachability res = new Reachability();
        Deque<FFunction> todoFunctions = new ArrayDeque<>(startingPoints);
        FClassVisitor reachabilityVisitor = reachabilityVisitor(todoFunctions);

        while (!todoFunctions.isEmpty()) {
            FFunction cur = todoFunctions.pollFirst();
            if (res.isReachable(cur))
                continue;

            res.addFunction(cur);
            if (cur.getMemberOf() instanceof OptionalNamespace && !cur.getIdentifier().equals(UnaryOperator.NOT.identifier)) { //TODO if we ever switch to optional handling in front end, this is no longer needed
                OptionalNamespace memberOf = (OptionalNamespace) cur.getMemberOf();
                todoFunctions.addFirst(memberOf.getOriginalFunction(cur));
                continue;
            }


            if (cur instanceof FieldAccessor) {
                //field
                FField field = ((FieldAccessor) cur).getField();

                if (field.getMemberOf().isNative())
                    handleType(field.getType(), res);

                if (!field.hasAssignment())
                    continue;

                //check whether the other accessor is reachable, if so the field was already seen
                if ((cur == field.getGetter() && res.isReachable(field.getSetter()))
                        || (cur == field.getSetter() && res.isReachable(field.getGetter())))
                    continue;

                //TODO when fields work in optionals, we need similar handling of optionals here as for functions below
                if (field.getMemberOf() instanceof FInstantiatedClass) {//for fields in instantiated classes, we have to visit the base instead because they are not yet baked
                    FInstantiatedClass instantiatedClass = (FInstantiatedClass) field.getMemberOf();
                    FField base;
                    if (field.isInstance()) {
                        base = instantiatedClass.getProxy().getInstanceFields().get(field.getIdentifier());
                    } else {
                        base = instantiatedClass.getProxy().getStaticFields().get(field.getIdentifier());
                    }
                    analyseBase(base).handle(instantiatedClass.getTypeInstantiation(), todoFunctions);
                } else { //normal field
                    field.accept(reachabilityVisitor);
                }
            } else {
                //Function
                if (cur.isNative()) {
                    for (FParameter p : cur.getSignature().getParameters())
                        handleType(p.getType(), res);
                    handleType(cur.getType(), res);
                }

                if (cur.isInstantiation()) { //for instantiated functions, we have to visit the base instead because they are not yet baked
                    analyseBase(cur.getBaseR()).handle(cur.getTypeInstantiationToBase(), todoFunctions);
                } else { //normal function
                    cur.accept(reachabilityVisitor);
                }
            }
        } //end while
        return res;
    }

    private static void handleType(FType type, Reachability reachability) {
        for (FType t : FTuple.unpackType(type))
            if (t instanceof FOptional)
                reachability.addNamespace(((FClass) ((FOptional) t).getBaseType()).getNamespace());
            else
                reachability.addNamespace(((FClass) t).getNamespace());
    }

    private static FClassVisitor reachabilityVisitor(Collection<FFunction> seenFunctions) {
        return new FClassVisitor() {

            @Override
            public boolean enterFunctionCall(FFunctionCall functionCall) {
                seenFunctions.add(functionCall.getFunction());
                return true;
            }
            @Override
            public FExpression visitFunctionAddress(FFunctionAddress address) {
                seenFunctions.add(address.getFunction());
                return address;
            }
        };
    }
    private static FClassVisitor reachabilityVisitor(BaseAnalysis baseAnalysis) {
        return new FClassVisitor() {
            BaseAnalysis current = baseAnalysis;

            @Override
            public boolean enterFunctionCall(FFunctionCall functionCall) {
                current.seenCalls.add(functionCall);
                return true;
            }
            @Override
            public FExpression visitFunctionAddress(FFunctionAddress address) {
                current.seenAddresses.add(address);
                return address;
            }
            @Override
            public void enterForEach(FForEach forEach) {
                assert forEach.getForImpl() instanceof PrimitiveFor;
                BaseAnalysis analysis = new BaseAnalysis();
                analysis.parentSegment = current;
                //b is the type of the for loop iter var, c is type of container
                FType loopVarType = forEach.getIterators().get(0).getType();
                FType containerType = getOnlyElement(((FFunctionCall) forEach.getContainer()).getArguments(false)).getType();
                current.subSegments.add(new Triple<>(analysis, (FTypeVariable) loopVarType, (FTypeVariable) containerType));
                current = analysis;
            }
            @Override
            public FStatement exitForEach(FForEach forEach, FExpression container, FStatement body) {
                assert forEach.getForImpl() instanceof PrimitiveFor;
                current = current.parentSegment;
                return null;
            }
        };
    }

    private static Map<FField, BaseAnalysis> cachedBaseFieldAnalysis = new HashMap<>();
    private static BaseAnalysis analyseBase(FField base) {
        BaseAnalysis res = cachedBaseFieldAnalysis.get(base);
        if (res == null) {
            res = new BaseAnalysis();
            base.accept(reachabilityVisitor(res));
            cachedBaseFieldAnalysis.put(base, res);
        }
        return res;
    }
    private static Map<FFunction, BaseAnalysis> cachedBaseFunctionAnalysis = new HashMap<>();
    private static BaseAnalysis analyseBase(FFunction base) {
        BaseAnalysis res = cachedBaseFunctionAnalysis.get(base);
        if (res == null) {
            res = new BaseAnalysis();
            base.accept(reachabilityVisitor(res));
            cachedBaseFunctionAnalysis.put(base, res);
        }
        return res;
    }

    private ReachableNamespace addNamespace(DefaultNamespace namespace) {
        return reachableNamespaces.computeIfAbsent(namespace, x -> new ReachableNamespace());
    }

    private void addFunction(FFunction function) {
        ReachableNamespace reachableNamespace = addNamespace((DefaultNamespace) function.getMemberOf());
        FInstantiatedFunction value = null;
        if (function instanceof FInstantiatedFunction) {
            FInstantiatedFunction fInstantiatedFunction = (FInstantiatedFunction) function;
            value = fInstantiatedFunction;
            function = fInstantiatedFunction.getProxy();
        }
        reachableNamespace.reachableFunctions.put(function, value);
    }

    @SuppressWarnings("SuspiciousMethodCalls")
    public boolean isReachable(FFunction function) {
        ReachableNamespace reachableNamespace = reachableNamespaces.get(function.getMemberOf());
        if (reachableNamespace == null)
            return false;
        return reachableNamespace.isReachable(function);
    }

    public Map<DefaultNamespace, ReachableNamespace> getReachableNamespaces() {
        return reachableNamespaces;
    }
}

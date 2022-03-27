package tys.frontier.passes.analysis.reachability;

import com.google.common.collect.MultimapBuilder;
import com.google.common.collect.SetMultimap;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.InstanceField;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionAddress;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FInstantiatedFunction;
import tys.frontier.code.function.FieldAccessor;
import tys.frontier.code.function.Signature;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.namespace.OptionalNamespace;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.loop.FForEach;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.statement.loop.forImpl.PrimitiveFor;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.visitor.FClassVisitor;
import tys.frontier.util.Utils;

import java.util.*;

import static com.google.common.collect.Iterables.getOnlyElement;
import static java.util.Collections.emptyMap;

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

        private static class SubSegment {
            FForEach loop;
            BaseAnalysis baseAnalysis;
            FTypeVariable containerType;
            FTypeVariable iteratorType;

            public SubSegment(FForEach loop, BaseAnalysis baseAnalysis, FTypeVariable containerType, FTypeVariable iteratorType) {
                this.loop = loop;
                this.baseAnalysis = baseAnalysis;
                this.containerType = containerType;
                this.iteratorType = iteratorType;
            }
        }


        BaseAnalysis parentSegment;
        List<SubSegment> subSegments = new ArrayList<>(); //special logic for primitive for loops that get lowered during baking
        Set<FFunctionCall> seenCalls = new HashSet<>();
        Set<FFunctionAddress> seenAddresses = new HashSet<>();

        private void handle(TypeInstantiation typeInstantiation, Collection<FFunction> seenFunctions) {
            assert typeInstantiation.values().stream().noneMatch(t -> t instanceof FTypeVariable);
            doHandle(typeInstantiation, seenFunctions);

            for (SubSegment subSegment : subSegments) {
                assert typeInstantiation.contains(subSegment.containerType);
                FClass instantiatedType = (FClass) typeInstantiation.getType(subSegment.containerType);

                if (subSegment.loop.getForImpl() instanceof PrimitiveFor) {
                    //special handling for optionals
                    if (instantiatedType instanceof FOptional)
                        instantiatedType = (FClass) ((FOptional) instantiatedType).getBaseType();

                    //special handling for arrays, because there is no "field" for the base type visit them manually
                    if (instantiatedType instanceof FArray)
                        subSegment.baseAnalysis.handle(typeInstantiation.with(subSegment.iteratorType, ((FArray) instantiatedType).getBaseType()), seenFunctions);

                    //this also handles tuples, since they have fields
                    for (FField field : instantiatedType.getInstanceFields().values()) {
                        subSegment.baseAnalysis.handle(typeInstantiation.with(subSegment.iteratorType, field.getType()), seenFunctions);
                    }
                } else {
                    Utils.cantHappen();
                }
            }
        }

        private void doHandle(TypeInstantiation typeInstantiation, Collection<FFunction> seenFunctions) {
            //Instantiate Base results and add to Q/reach
            for (FFunctionCall fC : seenCalls) {
                //We can't use fc.getArguments, because default Values have not been baked, so we work around it by finding the base signature and calling the formerly private fillDefaultArgs
                Signature sig = fC.getSignature().isLhs() ? fC.getFunction().getBaseR().getLhsSignature()
                        :  fC.getFunction().getBaseR().getSignature();
                List<FType> paramTypes = Utils.typesFromExpressionList(fC.getArgsAndFillDefaultFrom(sig), typeInstantiation::getType);

                FFunction f = Utils.findFunctionInstantiation(fC.getSignature(), paramTypes, emptyMap(), typeInstantiation, List.of()).getFunction();
                seenFunctions.add(f);
            }
            for (FFunctionAddress fA : seenAddresses) {
                List<FType> paramTypes = Utils.typesFromExpressionList(fA.getFunction().getSignature().getParameters(), typeInstantiation::getType);
                FFunction f = Utils.findFunctionInstantiation(fA.getFunction().getSignature(), paramTypes, emptyMap(), typeInstantiation, List.of()).getFunction();
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
        addFunction(FTypeType.fieldsOf);
        addNamespace(FFieldType.INSTANCE.getNamespace());
        addFunction(FFieldType.type.getGetter());
        addFunction(FFieldType.name.getGetter());
        addFunction(FFieldType.memberOf.getGetter());
        addFunction(FFieldType.INSTANCE.getConstructor());
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
            if (cur.getMemberOf() instanceof OptionalNamespace && !FOptional.isOriginalOptionalIdentifier(cur.getIdentifier())) { //TODO if we ever switch to optional handling in front end, this is no longer needed
                OptionalNamespace memberOf = (OptionalNamespace) cur.getMemberOf();
                todoFunctions.addFirst(memberOf.getOriginalFunction(cur));
                continue;
            }


            if (cur instanceof FieldAccessor) {
                //field
                FField field = ((FieldAccessor) cur).getField();

                if (field.getNamespace().getNative() != null)
                    handleType(field.getType(), res);

                if (!field.hasAssignment())
                    continue;

                //check whether the other accessor is reachable, if so the field was already seen
                if ((cur == field.getGetter() && res.isReachable(field.getSetter()))
                        || (cur == field.getSetter() && res.isReachable(field.getGetter())))
                    continue;

                //TODO when fields work in optionals, we need similar handling of optionals here as for functions below
                if (field.getNamespace().getType() instanceof FInstantiatedClass) {//for fields in instantiated classes, we have to visit the base instead because they are not yet baked
                    FInstantiatedClass instantiatedClass = (FInstantiatedClass) field.getNamespace().getType();
                    FField base;
                    if (field instanceof InstanceField)
                        base = instantiatedClass.getProxy().getInstanceFields().get(field.getIdentifier());
                    else
                        base = instantiatedClass.getProxy().getNamespace().getStaticFields().get(field.getIdentifier());
                    analyseBase(base).handle(instantiatedClass.getTypeInstantiation(), todoFunctions);
                } else { //normal field
                    field.accept(reachabilityVisitor);
                }
            } else {
                //Function
                if (cur.getNative() != null) {
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
                handleType(((FOptional) t).getBaseType(), reachability);
            else if (t instanceof CArray) //TODO this seems like it should not be here, reachability for native is nasty...
                handleType(((CArray) t).getBaseType(), reachability);
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
                ForImpl forImpl = forEach.getForImpl();
                assert forImpl instanceof PrimitiveFor;
                BaseAnalysis analysis = new BaseAnalysis();
                analysis.parentSegment = current;
                FType loopVarType = forEach.getIterators().get(0).getType();
                FType containerType = getOnlyElement(((FFunctionCall) forEach.getContainer()).getArguments(false)).getType();
                current.subSegments.add(new BaseAnalysis.SubSegment(forEach, analysis, (FTypeVariable) containerType, (FTypeVariable) loopVarType));
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

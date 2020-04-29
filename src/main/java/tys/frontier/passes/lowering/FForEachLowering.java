package tys.frontier.passes.lowering;

import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import tys.frontier.State;
import tys.frontier.code.FField;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.*;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.literal.FStringLiteral;
import tys.frontier.code.module.Module;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.statement.FAssignment;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarDeclaration;
import tys.frontier.code.statement.loop.FForEach;
import tys.frontier.code.statement.loop.FWhile;
import tys.frontier.code.statement.loop.forImpl.ForByIdx;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.statement.loop.forImpl.PrimitiveFor;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.passes.GenericBaking;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;

import static com.google.common.collect.Iterables.getOnlyElement;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.singletonList;
import static tys.frontier.util.Utils.mutableSingletonList;
import static tys.frontier.util.Utils.mutableSingletonMap;

public class FForEachLowering extends StatementReplacer {

    private static FForEachLowering INSTANCE = new FForEachLowering();

    private FForEachLowering() {}

    public static void lower(Module module) {
        module.accept(INSTANCE);
    }

    @Override
    public FStatement exitForEach(FForEach forEach, FExpression container, FStatement body) {
        return replace(forEach, currentFunction);
    }

    public FStatement replace (FForEach forEach, FFunction function) {
        ForImpl forImpl = forEach.getForImpl();
        if (forImpl instanceof ForByIdx)
            return buildForByIdx((ForByIdx) forImpl, forEach, function);
        else if (forImpl instanceof PrimitiveFor) {
            FExpression container = getOnlyElement(((FFunctionCall) forEach.getContainer()).getArguments(false));
            FType containerType = container.getType();
            if (containerType instanceof FTypeVariable) {
                if (currentFunction.getParameters().get(containerType.getIdentifier()) == containerType)
                    //primitive for will be lowered during baking
                    return forEach;
                else
                    return Utils.NYI("primitive for on non-baked TypeVariable");
            }
            FForEach pseudoForEach = FForEach.create(forEach.getNestedDepth(), forEach.getIdentifier(), forEach.getIterators(), forEach.getCounter().orElse(null), container, forEach.getBody());
            return buildPrimitiveFor((PrimitiveFor) forImpl, function, pseudoForEach);
        }
        else
            return Utils.NYI("ForImpl: " + forImpl);
    }

    private static FStatement buildForByIdx(ForByIdx forImpl, FForEach forEach, FFunction function) {
        return buildForByIdx(
                c -> FFunctionCall.createTrusted(forImpl.getGetSize().getSignature(), mutableSingletonList(c)),
                (c, i) -> mutableSingletonList(FFunctionCall.createTrusted(forImpl.getGetElement().getSignature(), asList(c, i))),
                forEach, function
        );
    }

    private static FStatement buildForByIdx(Function<FExpression,FExpression> getSize, BiFunction<FExpression, FExpression, List<FExpression>> getElement, FForEach forEach, FFunction function) {
        List<FStatement> res = new ArrayList<>(4);

        //first store the container expression in a local variable, if necessary
        FLocalVariable container = getContainer(forEach.getContainer(), function, res);

        //store the size in a local variable
        FLocalVariable size = function.getFreshVariable(FIntN._32);
        {
            FLocalVariableExpression containerAccess = new FLocalVariableExpression(container);
            res.add(FAssignment.createDecl(size, getSize.apply(containerAccess)));
        }

        //declare counter
        FLocalVariable counter = forEach.getCounter().orElse(function.getFreshVariable(FIntN._32));
        res.add(FAssignment.createDecl(counter, new FLiteralExpression(new FIntNLiteral(0))));

        //condition
        FExpression condition;
        {
            Signature less = BinaryOperator.LESS.getFunctionTrusted(FIntN._32, FIntN._32);
            condition = FFunctionCall.createTrusted(less, asList(new FLocalVariableExpression(counter), new FLocalVariableExpression(size)));
        }

        //as first statement of loop accessing the array and storing the result in the iterator var
        FStatement itDecl;
        {
            List<FExpression> decls = new ArrayList<>(forEach.getIterators().size());
            for (FLocalVariable it : forEach.getIterators()) {
                decls.add(new FVarDeclaration(it));
            }
            FLocalVariableExpression containerAccess = new FLocalVariableExpression(container);
            FLocalVariableExpression counterAccess = new FLocalVariableExpression(counter);
            itDecl = FAssignment.createTrusted(decls, getElement.apply(containerAccess, counterAccess));
        }

        //increment
        FStatement increment;
        {
            FLocalVariableExpression rhsCounterExp = new FLocalVariableExpression(counter);
            FLiteralExpression one = new FLiteralExpression(new FIntNLiteral(1));
            Signature plus = BinaryOperator.PLUS.getFunctionTrusted(counter.getType(), one.getType());
            FFunctionCall plusCall = FFunctionCall.createTrusted(plus, asList(rhsCounterExp, one));

            FLocalVariableExpression lhsCounterExp = new FLocalVariableExpression(counter);
            increment = FAssignment.createTrusted(singletonList(lhsCounterExp), mutableSingletonList(plusCall));
        }

        //Loop Body
        FBlock loopBody = FBlock.from(itDecl, forEach.getBody(), increment);

        //While Loop
        FWhile fWhile = FWhile.createTrusted(forEach.getNestedDepth(), forEach.getIdentifier(), condition, loopBody);
        forEach.getIdentifier().setLoop(fWhile);
        res.add(fWhile);

        return FBlock.from(res);
    }

    public static FStatement buildPrimitiveFor(PrimitiveFor forImpl, FFunction function, FForEach forEach)
    {
        List<FStatement> res = new ArrayList<>();

        //first store the container expression in a local variable, if necessary
        FLocalVariable container = getContainer(forEach.getContainer(), function, res);

        if (container.getType() instanceof FArray) {
            buildPrimitiveForArray(forImpl, function, forEach, container, res);
        } else if (container.getType() instanceof FTuple) {
            return Utils.NYI("tuple primitive for");
        } else if (container.getType() instanceof FOptional) {
            return Utils.NYI("optional primitive for");
        } else {
            buildPrimitiveForNormal(forImpl, forEach, container, res);
        }

        return FBlock.from(res);
    }

    private static void buildPrimitiveForNormal(PrimitiveFor forImpl, FForEach forEach, FLocalVariable container, List<FStatement> res) {
        FTypeVariable elementType = (FTypeVariable) ((FTuple) forImpl.getElementType()).getTypes().get(0);
        List<FLocalVariable> iterators = forEach.getIterators();
        assert iterators.size() == 2 && iterators.get(0).getType() == elementType && iterators.get(1).getType() == FFieldType.INSTANCE;

        FFunction fieldInfoGet = FArray.getArrayFrom(FFieldType.INSTANCE).getArrayGet();

        int i = 0;
        for (FField field : ((FClass) forEach.getContainer().getType()).getInstanceFields().values()) {

            //declare iterator and field
            FLocalVariable valVar = new FLocalVariable(iterators.get(0).getIdentifier(), field.getType());
            FVarDeclaration valDecl = new FVarDeclaration(valVar);
            FFunctionCall fieldGet = FFunctionCall.createTrusted(field.getGetter().getSignature(), mutableSingletonList(new FLocalVariableExpression(container)));

            FLocalVariable fieldVar = new FLocalVariable(iterators.get(1).getIdentifier(), FFieldType.INSTANCE);
            FVarDeclaration fieldVarDecl = new FVarDeclaration(fieldVar);

            FNamespaceExpression typeInfo = new FNamespaceExpression(forEach.getContainer().getType().getNamespace());
            FFunctionCall getFields = FFunctionCall.createTrusted(FTypeType.fields.getGetter().getSignature(), mutableSingletonList(typeInfo));
            FLiteralExpression idx = new FLiteralExpression(new FIntNLiteral(i));
            FFunctionCall fieldInfo = FFunctionCall.createTrusted(fieldInfoGet.getSignature(), asList(getFields, idx));

            FAssignment decl;
            Map<FLocalVariable, FLocalVariable> varMap;
            if (forEach.getCounter().isPresent()) {
                //declare counter
                FLocalVariable oldCounter = forEach.getCounter().get();
                FLocalVariable counterVar = new FLocalVariable(oldCounter.getIdentifier(), oldCounter.getType());
                FVarDeclaration counter = new FVarDeclaration(counterVar);
                FLiteralExpression counterVal = new FLiteralExpression(new FIntNLiteral(i));
                decl = FAssignment.createTrusted(asList(valDecl, fieldVarDecl, counter), asList(fieldGet, fieldInfo, counterVal));
                varMap = ImmutableMap.of(iterators.get(0), valVar, iterators.get(1), fieldVar, oldCounter, counterVar);
            } else {
                decl = FAssignment.createTrusted(asList(valDecl, fieldVarDecl), asList(fieldGet, fieldInfo));
                varMap = ImmutableMap.of(iterators.get(0), valVar, iterators.get(1), fieldVar);
            }

            //bake the body
            TypeInstantiation typeInstantiation = TypeInstantiation.create(mutableSingletonMap(elementType, field.getType()));
            FStatement bakedBody = GenericBaking.bake(forEach.getBody(), typeInstantiation, varMap);

            res.add(FBlock.from(decl, bakedBody));

            i++;
        }
    }



    private static void buildPrimitiveForArray(PrimitiveFor forImpl, FFunction function, FForEach forEach, FLocalVariable container, List<FStatement> res) {
        FTypeVariable elementType = (FTypeVariable) ((FTuple) forImpl.getElementType()).getTypes().get(0);
        List<FLocalVariable> iterators = forEach.getIterators();
        assert iterators.size() == 2 && iterators.get(0).getType() == elementType && iterators.get(1).getType() == FFieldType.INSTANCE;

        FArray containerType = (FArray) container.getType();

        iterators.get(0).setType(containerType.getBaseType());
        Signature intToString = getIntToString();

        //bake the body
        TypeInstantiation typeInstantiation = TypeInstantiation.create(mutableSingletonMap(elementType, containerType.getBaseType()));
        FStatement bakedBody = GenericBaking.bake(forEach.getBody(), typeInstantiation, emptyMap());

        FForEach proxy = FForEach.create(forEach.getNestedDepth(), forEach.getIdentifier(), iterators,
                forEach.getCounter().orElse(null), forEach.getContainer(), FBlock.from(bakedBody));

        //hurray for readable code, I am sorry
        FStatement forByIdx = buildForByIdx(
                c -> FFunctionCall.createTrusted(containerType.getSize().getGetter().getSignature(), mutableSingletonList(c)),
                (c, i) -> asList(
                        FFunctionCall.createTrusted(containerType.getArrayGet().getSignature(), asList(c, i)),
                        FFunctionCall.createTrusted(FFieldType.INSTANCE.getConstructor().getSignature(), asList(
                                FFunctionCall.createTrusted(intToString, mutableSingletonList(i)), //TODO
                                new FNamespaceExpression(containerType.getNamespace()),
                                new FNamespaceExpression(containerType.getNamespace())
                        ))
                ),
                proxy, function
        );

        res.add(forByIdx);
    }

    private static Signature getIntToString() {
        //not the most elegant solution, but it works
        FIdentifier stringsIdentifier = new FIdentifier("Strings");
        Namespace stringsNamespace = null;
        for (Module _import : State.get().getTypeModule().getImports()) {
            stringsNamespace = _import.getNamespace(stringsIdentifier);
            if (stringsNamespace != null)
                break;
        }
        assert stringsNamespace != null;
        try {
            return stringsNamespace.hardResolveFunction(new FIdentifier("intToString"),
                    singletonList(FIntN._64), ImmutableListMultimap.of(), FStringLiteral.TYPE, false
            ).signature;
        } catch (FunctionNotFound functionNotFound) {
            return Utils.handleException(functionNotFound);
        }
    }

    private static FLocalVariable getContainer(FExpression containerExpression, FFunction function, List<FStatement> res) {
        if (containerExpression instanceof FLocalVariableExpression) {
            return ((FLocalVariableExpression) containerExpression).getVariable();
        } else {
            FLocalVariable container = function.getFreshVariable(containerExpression.getType());
            res.add(FAssignment.createDecl(container, containerExpression));
            return container;
        }
    }


}

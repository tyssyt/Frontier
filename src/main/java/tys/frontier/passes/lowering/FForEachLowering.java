package tys.frontier.passes.lowering;

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
import tys.frontier.code.statement.*;
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
import static tys.frontier.code.literal.FIntNLiteral.INT32_0;
import static tys.frontier.util.Utils.*;

//TODO @PositionForGeneratedCode, see how debugging feels and whether we want to have the increment lines got to the head of the loop etc
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

    public static FStatement replace (FForEach forEach, FFunction function) {
        ForImpl forImpl = forEach.getForImpl();
        if (forImpl instanceof ForByIdx)
            return buildForByIdx((ForByIdx) forImpl, forEach, function);
        else if (forImpl instanceof PrimitiveFor) {
            FExpression container = getOnlyElement(((FFunctionCall) forEach.getContainer()).getArguments(false));
            FType containerType = container.getType();
            if (containerType instanceof FTypeVariable) {
                if (function.getParameters().get(containerType.getIdentifier()) == containerType)
                    //primitive for will be lowered during baking
                    return forEach;
                else
                    return Utils.NYI("primitive for on non-baked TypeVariable");
            }
            FForEach pseudoForEach = FForEach.create(forEach.getPosition(), forEach.getNestedDepth(), forEach.getIdentifier(), forEach.getIterators(), forEach.getCounter().orElse(null), container, forEach.getBody());
            return buildPrimitiveFor((PrimitiveFor) forImpl, function, pseudoForEach);
        }
        return Utils.NYI("ForImpl: " + forImpl);
    }

    private static FStatement buildForByIdx(ForByIdx forImpl, FForEach forEach, FFunction function) {
        List<FStatement> res = new ArrayList<>(4);
        //first store the container expression in a local variable, if necessary
        FLocalVariable container = getContainer(forEach.getContainer(), function, res);
        buildForByIdx(
                c -> FFunctionCall.create(null, forImpl.getGetSize().getSignature(), mutableSingletonList(c)),
                (c, i) -> mutableSingletonList(FFunctionCall.create(null, forImpl.getGetElement().getSignature(), asList(c, i))),
                forEach, function, container, res
        );
        return FBlock.from(forEach.getPosition(), res);
    }

    private static void buildForByIdx(Function<FExpression,FExpression> getSize, BiFunction<FExpression, FExpression,
            List<FExpression>> getElement, FForEach forEach, FFunction function, FLocalVariable container, List<FStatement> res) {
        //store the size in a local variable
        FLocalVariable size = function.getFreshVariable(null, FIntN._32);
        {
            FVariableExpression containerAccess = new FVariableExpression(null, container);
            res.add(FAssignment.createDecl(size, getSize.apply(containerAccess)));
        }

        //declare counter
        FLocalVariable counter = forEach.getCounter().orElseGet(() -> function.getFreshVariable(null, FIntN._32));
        res.add(FAssignment.createDecl(counter, new FLiteralExpression(null, INT32_0)));

        //condition
        FExpression condition;
        {
            Signature less = BinaryOperator.LESS.getFunctionTrusted(FIntN._32, FIntN._32);
            condition = FFunctionCall.create(null, less, asList(new FVariableExpression(null, counter), new FVariableExpression(null, size)));
        }

        //as first statement of loop accessing the array and storing the result in the iterator var
        FStatement itDecl;
        {
            List<FExpression> decls = map(forEach.getIterators(), FVarDeclaration::new);
            FExpression assignLhs = decls.size() == 1 ? getOnlyElement(decls) : new Pack(null, decls);
            FVariableExpression containerAccess = new FVariableExpression(null, container);
            FVariableExpression counterAccess = new FVariableExpression(null, counter);
            itDecl = FAssignment.createTrusted(null, List.of(assignLhs), getElement.apply(containerAccess, counterAccess));
        }

        //increment
        FStatement increment;
        {
            FVariableExpression rhsCounterExp = new FVariableExpression(null, counter);
            FLiteralExpression one = new FLiteralExpression(null, new FIntNLiteral(1, FIntN._32));
            Signature plus = BinaryOperator.PLUS.getFunctionTrusted(counter.getType(), one.getType());
            FFunctionCall plusCall = FFunctionCall.create(null, plus, asList(rhsCounterExp, one));

            FVariableExpression lhsCounterExp = new FVariableExpression(null, counter);
            increment = FAssignment.createTrusted(null, singletonList(lhsCounterExp), mutableSingletonList(plusCall));
        }

        //Loop Body
        FBlock loopBody = FBlock.from(forEach.getBody().getPosition(), itDecl, forEach.getBody(), increment);

        //While Loop
        FWhile fWhile = FWhile.createTrusted(forEach.getPosition(), forEach.getNestedDepth(), forEach.getIdentifier(), condition, loopBody);
        forEach.getIdentifier().setLoop(fWhile);
        res.add(fWhile);
    }

    public static FStatement buildPrimitiveFor(PrimitiveFor forImpl, FFunction function, FForEach forEach)
    {
        List<FStatement> res = new ArrayList<>();

        //first store the container expression in a local variable, if necessary
        FLocalVariable container = getContainer(forEach.getContainer(), function, res);

        buildPrimitiveFor(forImpl, function, forEach, container, res);

        return FBlock.from(forEach.getPosition(), res);
    }

    private static void buildPrimitiveFor(PrimitiveFor forImpl, FFunction function, FForEach forEach, FLocalVariable container, List<FStatement> res) {
        if (container.getType() instanceof FArray) {
            buildPrimitiveForArray(forImpl, function, forEach, container, res);
        } else if (container.getType() instanceof FTuple) {
            buildPrimitiveForNormal(forImpl, forEach, container, res);
        } else if (container.getType() instanceof FOptional) {
            buildPrimitiveForOptional(forImpl, function, forEach, container, res);
        } else {
            buildPrimitiveForNormal(forImpl, forEach, container, res);
        }
    }

    private static void buildPrimitiveForNormal(PrimitiveFor forImpl, FForEach forEach, FLocalVariable container, List<FStatement> res) {
        FTypeVariable elementType = (FTypeVariable) ((FTuple) forImpl.getElementType()).getTypes().get(0);
        List<FLocalVariable> iterators = forEach.getIterators();
        assert iterators.size() == 2 && iterators.get(0).getType() == elementType && iterators.get(1).getType() == FFieldType.INSTANCE;

        FFunction fieldInfoGet = FArray.getArrayFrom(FFieldType.INSTANCE).getArrayGet();

        int i = 0;
        for (FField field : ((FClass) forEach.getContainer().getType()).getInstanceFields().values()) {

            //declare iterator and field
            FLocalVariable valVar = new FLocalVariable(iterators.get(0).getPosition(), iterators.get(0).getIdentifier(), field.getType());
            FVarDeclaration valDecl = new FVarDeclaration(valVar);
            FFunctionCall fieldGet = FFunctionCall.create(null, field.getGetter().getSignature(), mutableSingletonList(new FVariableExpression(null, container)));

            FLocalVariable fieldVar = new FLocalVariable(iterators.get(1).getPosition(), iterators.get(1).getIdentifier(), FFieldType.INSTANCE);
            FVarDeclaration fieldVarDecl = new FVarDeclaration(fieldVar);

            FNamespaceExpression typeInfo = new FNamespaceExpression(null, forEach.getContainer().getType().getNamespace());
            FFunctionCall getFields = FFunctionCall.create(null, FTypeType.fields.getGetter().getSignature(), mutableSingletonList(typeInfo));
            FLiteralExpression idx = new FLiteralExpression(null, new FIntNLiteral(i, FIntN._32));
            FFunctionCall fieldInfo = FFunctionCall.create(null, fieldInfoGet.getSignature(), asList(getFields, idx));

            FAssignment decl;
            Map<FLocalVariable, FLocalVariable> varMap;
            if (forEach.getCounter().isPresent()) {
                //declare counter
                FLocalVariable oldCounter = forEach.getCounter().get();
                FLocalVariable counterVar = new FLocalVariable(oldCounter.getPosition(), oldCounter.getIdentifier(), oldCounter.getType());
                FVarDeclaration counter = new FVarDeclaration(counterVar);
                FLiteralExpression counterVal = new FLiteralExpression(null, new FIntNLiteral(i, FIntN._32));
                decl = FAssignment.createTrusted(null, asList(valDecl, fieldVarDecl, counter), asList(fieldGet, fieldInfo, counterVal));
                varMap = ImmutableMap.of(iterators.get(0), valVar, iterators.get(1), fieldVar, oldCounter, counterVar);
            } else {
                decl = FAssignment.createTrusted(null, asList(valDecl, fieldVarDecl), asList(fieldGet, fieldInfo));
                varMap = ImmutableMap.of(iterators.get(0), valVar, iterators.get(1), fieldVar);
            }

            //bake the body
            TypeInstantiation typeInstantiation = TypeInstantiation.create(mutableSingletonMap(elementType, field.getType()));
            FStatement bakedBody = GenericBaking.bake(forEach.getBody(), typeInstantiation, varMap);

            res.add(FBlock.from(forEach.getBody().getPosition(), decl, bakedBody));

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

        FForEach proxy = FForEach.create(forEach.getPosition(), forEach.getNestedDepth(), forEach.getIdentifier(), iterators,
                forEach.getCounter().orElse(null), forEach.getContainer(), FBlock.from(bakedBody));

        //hurray for readable code, I am sorry
        buildForByIdx(
                c -> FFunctionCall.create(null, containerType.getSize().getGetter().getSignature(), mutableSingletonList(c)),
                (c, i) -> asList(
                        FFunctionCall.create(null, containerType.getArrayGet().getSignature(), asList(c, i)),
                        FFunctionCall.create(null, FFieldType.INSTANCE.getConstructor().getSignature(), asList(
                                FFunctionCall.createForBaking(null, intToString, mutableSingletonList(i)), //TODO
                                new FNamespaceExpression(null, containerType.getNamespace()),
                                new FNamespaceExpression(null, containerType.getNamespace())
                        ))
                ),
                proxy, function, container, res
        );
    }



    private static void buildPrimitiveForOptional(PrimitiveFor forImpl, FFunction function, FForEach forEach, FLocalVariable container, List<FStatement> res) {
        FTypeVariable elementType = (FTypeVariable) ((FTuple) forImpl.getElementType()).getTypes().get(0);
        List<FLocalVariable> iterators = forEach.getIterators();
        assert iterators.size() == 2 && iterators.get(0).getType() == elementType && iterators.get(1).getType() == FFieldType.INSTANCE;

        //make optional concrete
        FOptional containerType = (FOptional) container.getType();
        FFunctionCall containerPromote = FFunctionCall.create(null, containerType.getExmark().getSignature(), mutableSingletonList(new FVariableExpression(null, container)));
        FLocalVariable promotedContainer = new FLocalVariable(container.getPosition(), container.getIdentifier(), containerType.getBaseType());
        FAssignment decl = FAssignment.createDecl(promotedContainer, containerPromote);

        //set then
        List<FStatement> then = new ArrayList<>();
        then.add(decl);
        buildPrimitiveFor(forImpl, function, forEach, promotedContainer, then);

        FIf fIf = FIf.createTrusted(forEach.getPosition(), new FVariableExpression(forEach.getContainer().getPosition(), container), FBlock.from(forEach.getPosition(), then), null);
        res.add(fIf);
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
            return stringsNamespace.resolveFunction(new FIdentifier("intToString"), singletonList(FIntN._64), emptyMap(), FStringLiteral.TYPE, false, List.of()).signature;
        } catch (FunctionNotFound functionNotFound) {
            return Utils.handleException(functionNotFound);
        }
    }

    private static FLocalVariable getContainer(FExpression containerExpression, FFunction function, List<FStatement> res) {
        if (containerExpression instanceof FVariableExpression) {
            return ((FVariableExpression) containerExpression).getVariable();
        } else {
            FLocalVariable container = function.getFreshVariable(containerExpression.getPosition(), containerExpression.getType());
            res.add(FAssignment.createDecl(container, containerExpression));
            return container;
        }
    }


}

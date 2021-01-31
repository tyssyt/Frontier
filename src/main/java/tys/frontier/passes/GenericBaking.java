package tys.frontier.passes;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import tys.frontier.code.*;
import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.function.*;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.statement.loop.forImpl.PrimitiveFor;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.typeInference.IsIterable;
import tys.frontier.code.visitor.FClassVisitor;
import tys.frontier.passes.lowering.FForEachLowering;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;

import static com.google.common.collect.Iterables.getOnlyElement;
import static java.util.Collections.emptySet;
import static tys.frontier.passes.GenericBaking.VariableMode.*;
import static tys.frontier.util.Utils.typesFromExpressionList;

/**
 * mhhhhhhh... cookies
 */
public class GenericBaking implements FClassVisitor {

    public enum VariableMode {
        USE_ORIGINAL,
        FALLBACK_ORIGINAL,
        NO_ORIGINAL
    }

    private TypeInstantiation typeInstantiation;

    private FField currentField;
    private FFunction currentFunction;
    private VariableMode variableMode;

    private Map<FLocalVariable, FLocalVariable> varMap = new HashMap<>();
    private Map<FLoopIdentifier, FLoopIdentifier> loopMap = new HashMap<>();

    public GenericBaking(TypeInstantiation typeInstantiation, VariableMode variableMode) {
        this.typeInstantiation = typeInstantiation;
        this.variableMode = variableMode;
    }

    // actually this should be a two pass visitor that first adds fields and then adds functions, but atm this works because of specific ways certain things are coded
    public static void bake (FInstantiatedClass instantiatedClass) {
        GenericBaking visitor = new GenericBaking(instantiatedClass.getTypeInstantiation(), NO_ORIGINAL);

        for (FField field : instantiatedClass.getInstanceFields().values()) {
            visitor.currentField = field;
            instantiatedClass.getProxy().getInstanceFields().get(field.getIdentifier()).accept(visitor);
        }
        for (FField field : instantiatedClass.getNamespace().getStaticFields().values()) {
            visitor.currentField = field;
            instantiatedClass.getProxy().getNamespace().getStaticFields().get(field.getIdentifier()).accept(visitor);
        }

        for (Signature signature : instantiatedClass.getNamespace().getFunctions(false).values()) {
            FFunction function = signature.getFunction();
            if (function.isConstructor() || function instanceof FieldAccessor || function.getIdentifier() == FConstructor.MALLOC_ID)
                continue;
            ClassInstantiationFunction instantiatedFunction = (ClassInstantiationFunction) function;
            visitor.currentFunction = instantiatedFunction;
            instantiatedFunction.getBaseR().accept(visitor);
            instantiatedFunction.setBaked();
        }

        //set default value for fields in constructor
        ImmutableList<FParameter> parameters = instantiatedClass.getConstructor().getSignature().getParameters();
        for (FParameter param : parameters) {
            if (!param.hasDefaultValue())
                continue;
            FField field = instantiatedClass.getInstanceFields().get(param.getIdentifier());
            if (field.hasAssignment()) {
                param.setDefaultValueTrusted(field.getAssignment().get(), emptySet()); //TODO dependencies between fields
            }
        }
    }

    public static void bake (FInstantiatedFunction instantiatedFunction) {
        GenericBaking visitor = new GenericBaking(instantiatedFunction.getTypeInstantiation(), NO_ORIGINAL);
        visitor.currentFunction = instantiatedFunction;
        instantiatedFunction.getProxy().accept(visitor);
        instantiatedFunction.setBaked();
        instantiatedFunction.getMemberOf().addFunctionTrusted(instantiatedFunction);
    }

    public static FStatement bake (FStatement statement) {
        GenericBaking visitor = new GenericBaking(TypeInstantiation.EMPTY, USE_ORIGINAL);
        return statement.accept(visitor);
    }

    public static FStatement bake (FStatement statement, TypeInstantiation typeInstantiation, Map<FLocalVariable, FLocalVariable> varMap) {
        GenericBaking baking = new GenericBaking(typeInstantiation, FALLBACK_ORIGINAL);
        baking.varMap.putAll(varMap);
        return statement.accept(baking);
    }

    @Override
    public void enterField(FField field) {
        if (field instanceof InstanceField) {
            assert currentField instanceof InstanceField;
            varMap.put(((InstanceField) field).getThis(), ((InstanceField) currentField).getThis());
        }
    }

    @Override
    public FField exitField(FField field, Optional<FExpression> assign) {
        assign.ifPresent(currentField::setAssignmentTrusted);
        varMap.clear();
        return currentField;
    }

    @Override
    public void enterFunction(FFunction function) {
        assert !function.isConstructor();

        //map parameters
        for (Pair<FParameter, FParameter> pair : Utils.zip(currentFunction.getSignature().getParameters(), function.getSignature().getParameters()))
            varMap.put(pair.b, pair.a);

        //bake default values
        for (Pair<FParameter, FParameter> pair : Utils.zip(currentFunction.getSignature().getParameters(), function.getSignature().getParameters())) {
            if (pair.a.hasDefaultValue()) {
                assert pair.b.hasDefaultValue();
                Set<FParameter> defaultValueDependencies = Utils.map(pair.b.getDefaultValueDependencies(), v -> (FParameter) varMap.get(v));
                pair.a.setDefaultValueTrusted(pair.b.getDefaultValue().accept(this), defaultValueDependencies);
            }
        }
    }
    @Override
    public FFunction exitFunction(FFunction function, Optional<FStatement> body) {
        varMap.clear();
        loopMap.clear();
        body.ifPresent(b -> currentFunction.setBody((FBlock) b));
        return currentFunction;
    }

    //Statements
    @Override
    public FStatement exitBlock(FBlock block, List<FStatement> fStatements) {
        return FBlock.from(block.getPosition(), fStatements);
    }

    @Override
    public FStatement exitExpressionStatement(FExpressionStatement statement, FExpression fExpression) {
        return new FExpressionStatement(statement.getPosition(), fExpression);
    }

    @Override
    public FStatement exitIf(FIf fIf, FExpression cond, FStatement then, Optional<FStatement> elze) {
        return FIf.createTrusted(fIf.getPosition(), cond, (FBlock) then, (FBlock) elze.orElse(null));
    }

    @Override
    public FStatement exitReturn(FReturn fReturn, List<FExpression> values) {
        if (variableMode == USE_ORIGINAL || variableMode == FALLBACK_ORIGINAL)
            return FReturn.createTrusted(fReturn.getPosition(), values, fReturn.getFunction());

        List<FExpression> returnExps = new ArrayList<>();
        List<FStatement> voidStatements = new ArrayList<>();
        for (FExpression value : values) {
            if (value.getType() == FTuple.VOID)
                voidStatements.add(new FExpressionStatement(value.getPosition(), value));
            else
                returnExps.add(value);
        }

        voidStatements.add(FReturn.createTrusted(fReturn.getPosition(), returnExps, currentFunction));
        return FBlock.from(fReturn.getPosition(), voidStatements);
    }

    @Override
    public FStatement exitVarAssignment(FAssignment assignment, List<FExpression> lhsExpressions, List<FExpression> values) {
        return FAssignment.createTrusted(assignment.getPosition(), lhsExpressions, values);
    }

    @Override
    public void enterWhile(FWhile fWhile) {
        loopMap.put(fWhile.getIdentifier(), new FLoopIdentifier());
    }

    @Override
    public void enterForEach(FForEach forEach) {
        ForImpl forImpl = forEach.getForImpl();
        if (forImpl instanceof PrimitiveFor || forImpl instanceof IsIterable) {
            //TODO for tuples & "normal" objects, we re-bake the body multiple times in the exit, so in theory the standard visit between enter and exit could be skipped
            //this code would need to be done always, but other cases may only appear in NO_ORIGINAL mode
            for (FLocalVariable old : forEach.getIterators()) {
                FLocalVariable iterator = new FLocalVariable(old.getPosition(), old.getIdentifier(), typeInstantiation.getType(old.getType()));
                varMap.put(old, iterator);
            }
            forEach.getCounter().ifPresent(old -> {
                FLocalVariable counter = new FLocalVariable(old.getPosition(), old.getIdentifier(), typeInstantiation.getType(old.getType()));
                varMap.put(old, counter);
            });
        }

        loopMap.put(forEach.getIdentifier(), new FLoopIdentifier());
    }

    @Override
    public FStatement exitWhile(FWhile fWhile, FExpression cond, FStatement body) {
        return FWhile.createTrusted(fWhile.getPosition(), fWhile.getNestedDepth(), loopMap.get(fWhile.getIdentifier()), cond, (FBlock) body);
    }

    @Override
    public FStatement exitForEach(FForEach forEach, FExpression container, FStatement body) {
        ForImpl forImpl = forEach.getForImpl();
        if (forImpl instanceof PrimitiveFor) {
            ArrayList<FLocalVariable> iterators = Utils.map(forEach.getIterators(), it -> varMap.get(it));
            FLocalVariable counter = forEach.getCounter().map(c -> varMap.get(c)).orElse(null);
            FExpression actualContainer = getOnlyElement(((FFunctionCall) container).getArguments(false));
            FForEach pseudoForEach = FForEach.create(forEach.getPosition(), forEach.getNestedDepth(), forEach.getIdentifier(), iterators, counter, actualContainer, FBlock.from(body));
            return FForEachLowering.buildPrimitiveFor((PrimitiveFor) forImpl, currentFunction, pseudoForEach);
        } else if (forImpl instanceof IsIterable) {
            //instantiate the forEach
            ArrayList<FLocalVariable> iterators = Utils.map(forEach.getIterators(), it -> varMap.get(it));
            FLocalVariable counter = forEach.getCounter().map(c -> varMap.get(c)).orElse(null);
            FForEach res = FForEach.create(forEach.getPosition(), forEach.getNestedDepth(), forEach.getIdentifier(), iterators, counter, container, FBlock.from(body));
            //lower it
            return FForEachLowering.replace(res, currentFunction);
        }

        assert variableMode == NO_ORIGINAL;
        List<FLocalVariable> iterators = new ArrayList<>(forEach.getIterators().size());
        for (FLocalVariable old : forEach.getIterators()) {
            varMap.put(old, new FLocalVariable(old.getPosition(), old.getIdentifier(), typeInstantiation.getType(old.getType())));
        }

        FLocalVariable counter = null;
        if (forEach.getCounter().isPresent()) {
            FLocalVariable old = forEach.getCounter().get();
            counter = new FLocalVariable(old.getPosition(), old.getIdentifier(), old.getType()); //no need for typeInstantiation, it's int32
            varMap.put(old, counter);
        }

        return FForEach.create(forEach.getPosition(), forEach.getNestedDepth(), loopMap.get(forEach.getIdentifier()), iterators, counter, container, (FBlock) body);
    }

    @Override
    public FOptElse exitOptElse(FOptElse optElse, FExpression optional, FExpression elze) {
        return FOptElse.createTrusted(optElse.getPosition(), optional, elze);
    }

    @Override
    public FCacheExpression exitCache(FCacheExpression cache, FExpression fExpression) {
        return Utils.NYI("Cache Expression baking"); //TODO
    }

    @Override
    public FArrayLiteral exitArrayLiteral(FArrayLiteral arrayLiteral, List<FExpression> elements) {
        return FArrayLiteral.createTrusted(arrayLiteral.getPosition(), typeInstantiation.getType(arrayLiteral.getType().getBaseType()), elements);
    }

    @Override
    public FStatement visitBreak(FBreak fBreak) {
        return new FBreak(fBreak.getPosition(), loopMap.get(fBreak.getLoop()));
    }

    @Override
    public FStatement visitContinue(FContinue fContinue) {
        return new FContinue(fContinue.getPosition(), loopMap.get(fContinue.getLoop()));
    }

    @Override
    public FExpression exitBrackets(FBracketsExpression brackets, FExpression inner) {
        return new FBracketsExpression(brackets.getPosition(), inner);
    }

    @Override
    public boolean enterFunctionCall(FFunctionCall functionCall) {
        return false; //do not visit defaults
    }

    @Override
    public FExpression exitFunctionCall(FFunctionCall functionCall, List<FExpression> params) {
        Signature signature = functionCall.getSignature();

        //params might contain null, so switch to keyword args
        ImmutableList<FParameter> originalParams = signature.getParameters();
        List<FType> paramTypes = new ArrayList<>(params.size());
        BitSet defaultArgs = new BitSet(params.size());

        for (int i = 0; i < params.size(); i++) {
            FExpression param = params.get(i);
            if (param == null) {
                paramTypes.add(typeInstantiation.getType(originalParams.get(i).getType()));
                defaultArgs.set(i);
            } else
                paramTypes.add(param.getType());
        }

        signature = Utils.findFunctionInstantiation(signature, paramTypes, ImmutableListMultimap.of(), typeInstantiation);
        return FFunctionCall.createUnpreparedTrusted(functionCall.getPosition(), signature, params, paramTypes, defaultArgs);
    }

    @Override
    public FExpression exitDynamicFunctionCall(DynamicFunctionCall functionCall, FExpression function, List<FExpression> params) {
        return DynamicFunctionCall.createTrusted(functionCall.getPosition(), function, params);
    }

    @Override
    public FExpression exitImplicitCast(FImplicitCast implicitCast, FExpression castedExpression) {
        return castedExpression;
    }

    @Override
    public FExpression visitLiteral(FLiteralExpression expression) {
        return new FLiteralExpression(expression.getPosition(), expression.getLiteral().copy());
    }

    @Override
    public FExpression visitVariable(FVariableExpression expression) { //TODO decl
        if (expression instanceof FVarDeclaration) {
            FLocalVariable old = expression.getVariable();
            switch (variableMode) {
                case USE_ORIGINAL:
                    return new FVarDeclaration(old);
                case FALLBACK_ORIGINAL: case NO_ORIGINAL:
                    FLocalVariable _new = new FLocalVariable(old.getPosition(), old.getIdentifier(), typeInstantiation.getType(old.getType()));
                    varMap.put(old, _new);
                    return new FVarDeclaration(_new);
                default:
                    return Utils.cantHappen();
            }
        }

        //in theory, the fallback_original call works in both other cases as well, but may hide errors
        return switch (variableMode) {
            case USE_ORIGINAL      -> new FVariableExpression(expression.getPosition(), expression.getVariable());
            case NO_ORIGINAL       -> new FVariableExpression(expression.getPosition(), varMap.get(expression.getVariable()));
            case FALLBACK_ORIGINAL -> new FVariableExpression(expression.getPosition(), varMap.getOrDefault(expression.getVariable(), expression.getVariable()));
        };
    }

    @Override
    public FExpression visitNamespaceExpression(FNamespaceExpression expression) {
        FType type = expression.getNamespace().getType();
        if (type == null)
            return expression;
        else
            return new FNamespaceExpression(expression.getPosition(), typeInstantiation.getType(type).getNamespace());
    }

    @Override
    public FExpression visitFunctionAddress(FFunctionAddress address) {
        Signature old = address.getFunction().getSignature();
        List<FType> argumentTypes = typesFromExpressionList(old.getParameters(), typeInstantiation::getType);
        Signature _new = Utils.findFunctionInstantiation(old, argumentTypes, ImmutableListMultimap.of(), typeInstantiation);
        return new FFunctionAddress(address.getPosition(), _new.getFunction());
    }
}

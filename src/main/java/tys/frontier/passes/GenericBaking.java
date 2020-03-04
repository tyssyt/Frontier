package tys.frontier.passes;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import tys.frontier.code.FField;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FExplicitCast;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.function.*;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.visitor.FClassVisitor;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;

import static java.util.Collections.emptySet;
import static tys.frontier.util.Utils.typesFromExpressionList;

/**
 * mhhhhhhh... cookies
 */
public class GenericBaking implements FClassVisitor {

    private TypeInstantiation typeInstantiation;

    private FField currentField;
    private FFunction currentFunction;
    private boolean useOriginal = false;

    private Map<FLocalVariable, FLocalVariable> varMap = new HashMap<>();
    private Map<FLoopIdentifier, FLoopIdentifier> loopMap = new HashMap<>();

    public GenericBaking(TypeInstantiation typeInstantiation) {
        this.typeInstantiation = typeInstantiation;
    }

    /*
            actually this should be a two pass visitor that first adds fields and then adds functions, but atm this works because of specific ways certain things are coded
         */
    public static void bake (FInstantiatedClass instantiatedClass) {
        GenericBaking visitor = new GenericBaking(instantiatedClass.getTypeInstantiation());

        //field has no instantiated version yet, but it is also much easier to find the corresponding field
        for (FField field : instantiatedClass.getFields()) {
            visitor.currentField = field;
            FField f;
            if (field.isInstance())
                f = instantiatedClass.getProxy().getInstanceFields().get(field.getIdentifier());
            else
                f = instantiatedClass.getProxy().getStaticFields().get(field.getIdentifier());
            f.accept(visitor);
        }

        for (Signature signature : instantiatedClass.getFunctions(false).values()) {
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
        GenericBaking visitor = new GenericBaking(instantiatedFunction.getTypeInstantiation());
        visitor.currentFunction = instantiatedFunction;
        instantiatedFunction.getProxy().accept(visitor);
        instantiatedFunction.setBaked();
        ((FClass) instantiatedFunction.getMemberOf()).addFunctionTrusted(instantiatedFunction);
    }

    public static FStatement bake (FStatement statement) {
        GenericBaking visitor = new GenericBaking(TypeInstantiation.EMPTY);
        visitor.useOriginal = true;
        return statement.accept(visitor);
    }

    public static FExpression bake (FExpression expression, TypeInstantiation typeInstantiation) {
        return expression.accept(new GenericBaking(typeInstantiation));
    }

    public static FExpression bake (FExpression expression, TypeInstantiation typeInstantiation, Map<FLocalVariable, FLocalVariable> varMap) {
        GenericBaking baking = new GenericBaking(typeInstantiation);
        baking.varMap.putAll(varMap);
        return expression.accept(baking);
    }

    @Override
    public void enterField(FField field) {
        if (field.isInstance())
            varMap.put(field.getThis(), currentField.getThis());
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
        return FBlock.from(fStatements);
    }

    @Override
    public FStatement exitExpressionStatement(FExpressionStatement statement, FExpression fExpression) {
        return new FExpressionStatement(fExpression);
    }

    @Override
    public FStatement exitIf(FIf fIf, FExpression cond, FStatement then, Optional<FStatement> elze) {
        return FIf.createTrusted(cond, (FBlock) then, (FBlock) elze.orElse(null));
    }

    @Override
    public FStatement exitReturn(FReturn fReturn, List<FExpression> values) {
        if (useOriginal)
            return FReturn.createTrusted(values, fReturn.getFunction());

        List<FExpression> returnExps = new ArrayList<>();
        List<FStatement> voidStatements = new ArrayList<>();
        for (FExpression value : values) {
            if (value.getType() == FTuple.VOID)
                voidStatements.add(new FExpressionStatement(value));
            else
                returnExps.add(value);
        }

        voidStatements.add(FReturn.createTrusted(returnExps, currentFunction));
        return FBlock.from(voidStatements);
    }

    @Override
    public FStatement exitVarAssignment(FAssignment assignment, List<FExpression> lhsExpressions, List<FExpression> values) {
        return FAssignment.createTrusted(lhsExpressions, values);
    }

    @Override
    public void enterWhile(FWhile fWhile) {
        loopMap.put(fWhile.getIdentifier(), new FLoopIdentifier());
    }

    @Override
    public void enterForEach(FForEach forEach) {
        loopMap.put(forEach.getIdentifier(), new FLoopIdentifier());
    }

    @Override
    public FStatement exitWhile(FWhile fWhile, FExpression cond, FStatement body) {
        return FWhile.createTrusted(fWhile.getNestedDepth(), loopMap.get(fWhile.getIdentifier()), cond, (FBlock) body);
    }

    @Override
    public FStatement exitForEach(FForEach forEach, FExpression container, FStatement body) {
        assert !useOriginal;

        List<FLocalVariable> iterators = new ArrayList<>(forEach.getIterators().size());
        for (FLocalVariable old : forEach.getIterators()) {
            varMap.put(old, new FLocalVariable(old.getIdentifier(), typeInstantiation.getType(old.getType())));
        }

        FLocalVariable counter = null;
        if (forEach.getCounter().isPresent()) {
            FLocalVariable old = forEach.getCounter().get();
            counter = new FLocalVariable(old.getIdentifier(), old.getType()); //no need for typeInstantiation, it's int32
            varMap.put(old, counter);
        }

        return FForEach.create(forEach.getNestedDepth(), loopMap.get(forEach.getIdentifier()), iterators, counter, container, (FBlock) body);
    }

    @Override
    public FStatement visitBreak(FBreak fBreak) {
        return new FBreak(loopMap.get(fBreak.getLoop()));
    }

    @Override
    public FStatement visitContinue(FContinue fContinue) {
        return new FContinue(loopMap.get(fContinue.getLoop()));
    }

    @Override
    public FExpression exitBrackets(FBracketsExpression brackets, FExpression inner) {
        return new FBracketsExpression(inner);
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
        return FFunctionCall.createUnpreparedTrusted(signature, params, paramTypes, defaultArgs);
    }

    @Override
    public FExpression exitDynamicFunctionCall(DynamicFunctionCall functionCall, FExpression function, List<FExpression> params) {
        return DynamicFunctionCall.createTrusted(function, params);
    }

    @Override
    public FExpression exitImplicitCast(FImplicitCast implicitCast, FExpression castedExpression) {
        return castedExpression;
    }

    @Override
    public FExpression exitExplicitCast(FExplicitCast explicitCast, FExpression castedExpression) {
        FType targetType = typeInstantiation.getType(explicitCast.getType());
        if (targetType != castedExpression.getType())
            return FExplicitCast.createTrusted(targetType, castedExpression);
        else
            return castedExpression;
    }

    @Override
    public FExpression visitLiteral(FLiteralExpression expression) {
        return new FLiteralExpression(expression.getLiteral().copy());
    }

    @Override
    public FExpression visitVariable(FLocalVariableExpression expression) { //TODO decl
        if (expression instanceof FVarDeclaration) {
            FLocalVariable old = expression.getVariable();
            FLocalVariable _new;
            if(useOriginal)
                _new = old;
            else
                _new = new FLocalVariable(old.getIdentifier(), typeInstantiation.getType(old.getType()));
            varMap.put(old, _new);
            return new FVarDeclaration(_new);
        }

        return new FLocalVariableExpression(useOriginal ? expression.getVariable() : varMap.get(expression.getVariable()));
    }

    @Override
    public FExpression visitClassExpr(FClassExpression expression) {
        return new FClassExpression(typeInstantiation.getType(expression.getfClass()));
    }

    @Override
    public FExpression visitFunctionAddress(FFunctionAddress address) {
        Signature old = address.getFunction().getSignature();
        List<FType> argumentTypes = typesFromExpressionList(old.getParameters(), typeInstantiation::getType);
        Signature _new = Utils.findFunctionInstantiation(old, argumentTypes, ImmutableListMultimap.of(), typeInstantiation);
        return new FFunctionAddress(_new.getFunction());
    }
}

package tys.frontier.passes;

import com.google.common.collect.ImmutableListMultimap;
import tys.frontier.code.FField;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FExplicitCast;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.function.*;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.FClassVisitor;
import tys.frontier.util.Utils;

import java.util.*;

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
            if (function.isConstructor() ||function.getIdentifier() == FConstructor.MALLOC_ID)
                continue;
            ClassInstantiationFunction instantiatedFunction = (ClassInstantiationFunction) function;
            visitor.currentFunction = instantiatedFunction;
            instantiatedFunction.getBaseR().accept(visitor);
            instantiatedFunction.setBaked();
        }

        //set default value for fields in constructor
        for (FParameter param : instantiatedClass.getConstructor().getSignature().getParameters()) {
            if (!param.hasDefaultValue())
                continue;
            FField field = instantiatedClass.getInstanceFields().get(param.getIdentifier());
            if (field.hasAssignment())
                param.setDefaultValueTrusted(field.getAssignment().get());
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
        for (int i = 0; i < function.getSignature().getParameters().size(); i++) {
            FParameter p = currentFunction.getSignature().getParameters().get(i);
            FParameter old = function.getSignature().getParameters().get(i);
            varMap.put(old, p);
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
    public FStatement exitVarAssignment(FVarAssignment assignment, List<FExpression> variables, List<FExpression> values) {
        //noinspection unchecked,rawtypes
        return FVarAssignment.createTrusted(((List) variables), assignment.getOperator(), values);
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

    //Expressions
    @Override
    public FExpression exitArrayAccess(FArrayAccess arrayAccess, FExpression array, FExpression index) {
        return FArrayAccess.createTrusted(array, index);
    }

    @Override
    public FExpression exitBrackets(FBracketsExpression brackets, FExpression inner) {
        return new FBracketsExpression(inner);
    }

    @Override
    public FExpression exitFunctionCall(FFunctionCall functionCall, List<FExpression> params) {
        FFunction function = functionCall.getFunction();

        FFunctionIdentifier identifier = function.getIdentifier();
        if (function.getMemberOf() instanceof FPredefinedClass &&
                (identifier.equals(UnaryOperator.INC.identifier) || identifier.equals(UnaryOperator.DEC.identifier))
        ) {
            //special case for inc and dec on predefined types, they are both write and read //TODO I don't like this here
            ((FVariableExpression) params.get(0)).setAccessType(FVariableExpression.AccessType.LOAD_AND_STORE);
        }

        function = Utils.findFunctionInstantiation(function, Utils.typesFromExpressionList(params, typeInstantiation::getType), ImmutableListMultimap.of(), typeInstantiation);
        return FFunctionCall.createTrusted(function, params);
    }

    @Override
    public FExpression exitDynamicFunctionCall(DynamicFunctionCall functionCall, FExpression function, List<FExpression> params) {
        return DynamicFunctionCall.createTrusted(function, params);
    }

    @Override
    public FExpression exitFieldAccess(FFieldAccess fieldAccess, FExpression object) {
        FField old = fieldAccess.getField();
        FField field = Utils.findFieldInstantiation(old, typeInstantiation);
        if (fieldAccess.isStatic())
            return FFieldAccess.createStatic(field);
        else {
            return FFieldAccess.createInstanceTrusted(field, object);
        }
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
        return new FClassExpression(typeInstantiation.getType(expression.getType()));
    }

    @Override
    public FExpression visitFunctionAddress(FFunctionAddress address) {
        FFunction old = address.getFunction();
        List<FType> argumentTypes = Utils.typesFromExpressionList(old.getSignature().getParameters(), typeInstantiation::getType);
        FFunction function = Utils.findFunctionInstantiation(old, argumentTypes, ImmutableListMultimap.of(), typeInstantiation);
        return new FFunctionAddress(function);
    }
}

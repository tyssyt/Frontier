package tys.frontier.passes;

import tys.frontier.code.*;
import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FExplicitCast;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.function.ClassInstantiationFunction;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FInstantiatedFunction;
import tys.frontier.code.function.operator.FUnaryOperator;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.visitor.FClassVisitor;
import tys.frontier.util.Utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

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
                f = instantiatedClass.getBaseClass().getInstanceFields().get(field.getIdentifier());
            else
                f = instantiatedClass.getBaseClass().getStaticFields().get(field.getIdentifier());
            f.accept(visitor);
        }

        for (FFunction function : instantiatedClass.getFunctions().values()) {
            if (function.isConstructor() ||function.getIdentifier() == FConstructor.MALLOC_ID)
                continue;
            ClassInstantiationFunction instantiatedFunction = (ClassInstantiationFunction) function;
            visitor.currentFunction = instantiatedFunction;
            instantiatedFunction.getBaseR().accept(visitor);
            instantiatedFunction.setBaked();
        }

        //set default value for fields in constructor
        for (FParameter param : instantiatedClass.getConstructor().getParams()) {
            if (!param.hasDefaultValue())
                continue;
            FField field = instantiatedClass.getInstanceFields().get(param.getIdentifier());
            if (field.hasAssignment())
                param.setDefaultValueTrusted(field.getAssignment().get());
        }

        instantiatedClass.setBaked();
    }

    public static void bake (FInstantiatedFunction instantiatedFunction) {
        assert !(instantiatedFunction.getMemberOf() instanceof FInstantiatedClass);
        GenericBaking visitor = new GenericBaking(instantiatedFunction.getTypeInstantiation());
        visitor.currentFunction = instantiatedFunction;
        instantiatedFunction.getBaseR().accept(visitor);
        instantiatedFunction.setBaked();
        ((FClass) instantiatedFunction.getMemberOf()).addFunctionTrusted(instantiatedFunction);
    }

    public static FStatement bake (FStatement statement) {
        GenericBaking visitor = new GenericBaking(TypeInstantiation.EMPTY);
        visitor.useOriginal = true;
        return statement.accept(visitor);
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
        for (int i = 0; i < function.getParams().size(); i++) {
            FParameter p = currentFunction.getParams().get(i);
            FParameter old = function.getParams().get(i);
            varMap.put(old, p);
            if (p.hasDefaultValue())
                //noinspection OptionalGetWithoutIsPresent
                p.setDefaultValueTrusted(old.getDefaultValue().get().accept(this));
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
    public FStatement exitReturn(FReturn fReturn, Optional<FExpression> value) {
        if (useOriginal)
            return FReturn.createTrusted(value.orElse(null), fReturn.getFunction());

        if (currentFunction.getType() == FVoid.INSTANCE && value.isPresent()) {
            /* the return value was a type parameter that got instantiated with void
               now we have a statement of the form return expr;
               this is invalid for a void, so we need to change it to {expr; return;}
             */
            return FBlock.from(
                    new FExpressionStatement(value.get()),
                    FReturn.createTrusted(null, currentFunction)
            );
        }
        return FReturn.createTrusted(value.orElse(null), currentFunction);
    }

    @Override
    public FStatement exitVarDeclaration(FVarDeclaration declaration, Optional<FExpression> value) {
        FLocalVariable old = declaration.getVar();
        FLocalVariable _new;
        if(useOriginal)
            _new = old;
        else
            _new = new FLocalVariable(old.getIdentifier(), typeInstantiation.getType(old.getType()));
        varMap.put(old, _new);
        return FVarDeclaration.createTrusted(_new, value.orElse(null));
    }

    @Override
    public FStatement exitVarAssignment(FVarAssignment assignment, FExpression variable, FExpression value) {
        return FVarAssignment.createTrusted((FVariableExpression) variable, assignment.getOperator(), value);
    }

    @Override
    public void enterWhile(FWhile fWhile) {
        loopMap.put(fWhile.getIdentifier(), new FLoopIdentifier());
    }

    @Override
    public void enterFor(FFor fFor) {
        loopMap.put(fFor.getIdentifier(), new FLoopIdentifier());
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
    public FStatement exitFor(FFor fFor, Optional<FStatement> declaration, Optional<FExpression> condition, Optional<FExpression> increment, FStatement body) {
        return FFor.createTrusted(fFor.getNestedDepth(), loopMap.get(fFor.getIdentifier()),
                (FVarDeclaration) declaration.orElse(null), condition.orElse(null), increment.orElse(null),
                (FBlock) body);
    }

    @Override
    public FStatement exitForEach(FForEach forEach, FExpression container, FStatement body) {
        FLocalVariable old = forEach.getIterator();
        FLocalVariable _new = new FLocalVariable(old.getIdentifier(), typeInstantiation.getType(old.getType()));
        varMap.put(old, _new);
        return FForEach.create(forEach.getNestedDepth(), loopMap.get(forEach.getIdentifier()), _new, container, (FBlock) body);
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

        if (function instanceof FUnaryOperator) {
            FFunctionIdentifier identifier = function.getIdentifier();
            if (function.getMemberOf() instanceof FPredefinedClass &&
                    (identifier.equals(FUnaryOperator.Pre.INC.identifier) || identifier.equals(FUnaryOperator.Pre.DEC.identifier))
            ) {
                //special case for inc and dec on predefined types, they are both write and read //TODO I don't like this here
                ((FVariableExpression) params.get(0)).setAccessType(FVariableExpression.AccessType.LOAD_AND_STORE);
            }
        }

        function = Utils.findFunctionInstantiation(function, Utils.typesFromExpressionList(params, typeInstantiation::getType), typeInstantiation);
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
        return FExplicitCast.createTrusted(typeInstantiation.getType(explicitCast.getType()), castedExpression);
    }

    @Override
    public FExpression visitLiteral(FLiteralExpression expression) {
        return new FLiteralExpression(expression.getLiteral().copy());
    }

    @Override
    public FExpression visitVariable(FLocalVariableExpression expression) {
        return new FLocalVariableExpression(useOriginal ? expression.getVariable() : varMap.get(expression.getVariable()));
    }

    @Override
    public FExpression visitClassExpr(FClassExpression expression) {
        return new FClassExpression(typeInstantiation.getType(expression.getType()));
    }

    @Override
    public FExpression visitFunctionAddress(FFunctionAddress address) {
        FFunction old = address.getFunction();
        List<FType> argumentTypes = Utils.typesFromExpressionList(old.getParams(), typeInstantiation::getType);
        FFunction function = Utils.findFunctionInstantiation(old, argumentTypes, typeInstantiation);
        return new FFunctionAddress(function);
    }
}

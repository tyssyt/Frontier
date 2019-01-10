package tys.frontier.passes;

import com.google.common.collect.Iterables;
import tys.frontier.code.*;
import tys.frontier.code.Operator.FUnaryOperator;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.visitor.FClassVisitor;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

import java.util.*;

/**
 * mhhhhhhh... cookies
 */
public class GenericBaking implements FClassVisitor {

    private TypeInstantiation typeInstantiation;

    private FInstantiatedClass currentClass;
    private FFunction currentFunction;

    private Map<FLocalVariable, FLocalVariable> varMap = new HashMap<>();
    private Map<FLoopIdentifier, FLoopIdentifier> loopMap = new HashMap<>();
    private Map<FField, FField> fieldMap = new HashMap<>();

    private GenericBaking(FInstantiatedClass instantiatedClass) {
        typeInstantiation = instantiatedClass.getTypeInstantiation();
        currentClass = instantiatedClass;
    }

    /*
        actually this should be a two pass visitor that first adds fields and then adds functions, but atm this works because of specific ways certain things are coded
     */
    public static void bake (FInstantiatedClass base) {
        base.getBaseClass().accept(new GenericBaking(base));
    }

    @Override
    public FInstantiatedClass exitType(FType fClass, List<FField> fFields, List<FFunction> fFunctions) {
        //default functions?
        try {
            for (FField field : fFields) {
                currentClass.addField(field);
            }
            for (FFunction function : fFunctions) {
                currentClass.addFunction(function);
            }
        } catch (IdentifierCollision | SignatureCollision e) {
            Utils.handleException(e);
        }

        //the constructor we generate by baking the generic constructor is not of class FConstructor, so remove it and generate one the normal way
        Collection<FFunction> constructors = currentClass.getFunctions().get(FConstructor.IDENTIFIER);
        FFunction oldConstructor = Iterables.getOnlyElement(constructors);
        constructors.clear();
        currentClass.getFunctions().get(FConstructor.MALLOC_ID).clear(); //TODO this entire block should be done very differently, constructor should be a flag we can just set
        currentClass.setConstructorVisibility(((FClass) fClass).getConstructorVisibility());
        FConstructor newConstructor = currentClass.generateConstructor();
        for (FFunctionCall functionCall : oldConstructor.getCalledBy()) {
            functionCall.setFunction(newConstructor);
        }
        return currentClass;
    }

    @Override
    public void enterField(FField field) {
        FField currentField = new FField(field.getIdentifier(), typeInstantiation.getType(field.getType()), currentClass, field.getVisibility(), !field.isInstance(), field.hasAssignment());
        fieldMap.put(field, currentField);
        if (field.isInstance())
            varMap.put(field.getThis(), currentField.getThis());
    }

    @Override
    public FField exitField(FField field, Optional<FExpression> assign) {
        FField res = fieldMap.get(field);
        assign.ifPresent(res::setAssignmentTrusted);
        varMap.clear();
        return res;
    }

    @Override
    public void enterFunction(FFunction function) {
        if (function.isConstructor()) {} //TODO no clue, can we skip?
        currentFunction = currentClass.getInstantiatedFunction(function);
        for (int i = 0; i < function.getParams().size(); i++) {
            FParameter p = currentFunction.getParams().get(i);
            FParameter old = function.getParams().get(i);
            varMap.put(old, p);
            if (p.hasDefaultValue())
                //noinspection OptionalGetWithoutIsPresent
                p.setDefaultValueTrusted(old.getDefaultValue().get().accept(this));
        }
        // old.getDefaultValue().map(dv -> dv.accept(this)).orElse(null)
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
        return FReturn.createTrusted(value.orElse(null), currentFunction);
    }

    @Override
    public FStatement exitVarDeclaration(FVarDeclaration declaration, Optional<FExpression> value) {
        FLocalVariable old = declaration.getVar();
        FLocalVariable _new = new FLocalVariable(old.getIdentifier(), typeInstantiation.getType(old.getType()));
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

        function = bakeFunction(function);
        return FFunctionCall.createTrusted(function, params);
    }

    @Override
    public FExpression exitDynamicFunctionCall(DynamicFunctionCall functionCall, FExpression function, List<FExpression> params) {
        return DynamicFunctionCall.createTrusted(function, params);
    }

    private FFunction bakeFunction(FFunction function) {
        if (function.getMemberOf() == currentClass.getBaseClass())
            function = currentClass.getInstantiatedFunction(function);
        if (function.getMemberOf() instanceof FArray) //TODO I really don't like this explicit handling of functions, and it will have to be expanded if we allow TypeVariable to have functions
            function = Utils.getFunctionInClass(function, (FClass) typeInstantiation.getType(function.getMemberOf()));
        return function;
    }

    @Override
    public FExpression exitFieldAccess(FFieldAccess fieldAccess, FExpression object) {
        FField field = fieldAccess.getField();
        if (field.getMemberOf() == currentClass.getBaseClass())
            field = fieldMap.get(field);
        if (field.getMemberOf() instanceof FArray) //TODO I really don't like this explicit handling of fields, and it will have to be expanded if we allow TypeVariable to have fields
            field = typeInstantiation.getType(field.getMemberOf()).getInstanceFields().get(FArray.SIZE);
        if (fieldAccess.isStatic())
            return FFieldAccess.createStatic(field);
        else {
            return FFieldAccess.createInstanceTrusted(field, object);
        }
    }

    @Override
    public FExpression exitImplicitCast(FImplicitCast implicitCast, FExpression castedExpression) {
        return FImplicitCast.createTrusted(typeInstantiation.getType(implicitCast.getType()), castedExpression);
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
        return new FLocalVariableExpression(varMap.get(expression.getVariable()));
    }

    @Override
    public FExpression visitClassExpr(FClassExpression expression) {
        return new FClassExpression(typeInstantiation.getType(expression.getType()));
    }

    @Override
    public FExpression visitFunctionAddress(FFunctionAddress address) { //TODO this is my best guess to what should happen, haven't put much thought into it though
        FFunction function = bakeFunction(address.getFunction());
        return new FFunctionAddress(function);
    }
}

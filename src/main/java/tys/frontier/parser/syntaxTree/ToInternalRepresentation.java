package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Multimap;
import tys.frontier.code.*;
import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FExplicitCast;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.InstantiableFunctionCopy;
import tys.frontier.code.function.operator.FUnaryOperator;
import tys.frontier.code.identifier.*;
import tys.frontier.code.literal.FLambda;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.parser.warnings.UnreachableStatements;
import tys.frontier.parser.warnings.Warning;
import tys.frontier.passes.GenericBaking;
import tys.frontier.util.MapStack;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;

public class ToInternalRepresentation extends FrontierBaseVisitor {

    private static class FunctionContext {
        FFunction function;
        Deque<FLoopIdentifier> loops = new ArrayDeque<>();
        MapStack<FIdentifier, FLocalVariable> declaredVars = new MapStack<>();
        Set<FTypeVariable> genericFunctionAddressToInstantiate = new HashSet<>();

        FunctionContext(FFunction function) {
            this.function = function;
        }
    }

    private SyntaxTreeData treeData;
    private List<Warning> warnings = new ArrayList<>();
    private List<SyntaxError> errors = new ArrayList<>();

    private FClass currentType;
    private Map<FTypeIdentifier, FTypeVariable> currentTypeParams;
    private Deque<FunctionContext> functionContextStack = new ArrayDeque<>();
    private Map<FVariable, FTypeVariable> typeVariableMap = new HashMap<>();

    private Module module;


    private ToInternalRepresentation(SyntaxTreeData syntaxTreeData, Module module) {
        this.treeData = syntaxTreeData;
        this.module = module;
    }

    public static List<Warning> toInternal(SyntaxTreeData syntaxTreeData, Module module) throws SyntaxErrors {
        ToInternalRepresentation visitor = new ToInternalRepresentation(syntaxTreeData, module);
        try {
            visitor.generateConstructors();
            visitor.visitFile(syntaxTreeData.root);
            if (!visitor.errors.isEmpty())
                throw SyntaxErrors.create(visitor.errors);
            return visitor.warnings;
        } catch (AssertionError assertionError) {
            if (!visitor.errors.isEmpty())
                throw SyntaxErrors.create(visitor.errors);
            throw assertionError;
        }
    }

    public void generateConstructors() {
        for (FClass fClass : treeData.classes.values()) {
            fClass.generateConstructor();
        }
    }

    private FunctionContext currentFunction() {
        return functionContextStack.getLast();
    }

    private FType findType(FTypeIdentifier identifier) {
        //first check declared and imported classes
        FType res = module.getClasses().get(identifier);
        if (res != null)
            return res;
        res = module.getImportedClasses().get(identifier);
        if (res != null)
            return res;
        //check type parameters of current class
        res = currentTypeParams.get(identifier);
        if (res != null)
            return res;
        //check type parameters of current function
        res = currentFunction().function.getParameters().get(identifier);
        if (res != null)
            return res;
        //check for declaration of type variables
        try {
            FVariable variable = findLocal(identifier).getVariable();
            return typeVariableMap.get(variable);
        } catch (UndeclaredVariable ignored) {}
        return null;
    }

    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentType = treeData.classes.get(ctx);
        //noinspection unchecked
        currentTypeParams = Utils.asTypeMap((List<FTypeVariable>)currentType.getParametersList());
        //handle typeParameterSpecification
        for (FrontierParser.TypeParameterSpecificationContext c : ctx.typeParameterSpecification()) {
            try {
                ParserContextUtils.handleTypeParameterSpecification(c, currentTypeParams, this::findType);
            } catch (SyntaxError syntaxError) {
                errors.add(syntaxError);
            }
        }
        return visitChildren(ctx);
    }

    //fields
    @Override
    public FField visitFieldDeclaration(FrontierParser.FieldDeclarationContext ctx) {
        FField field = treeData.fields.get(ctx);
        if (ctx.expression() != null) {
            functionContextStack.addLast(new FunctionContext(null));
            try {
                currentFunction().declaredVars.push();
                if (field.isInstance()) {
                    FLocalVariable _this = field.getThis();
                    currentFunction().declaredVars.put(_this.getIdentifier(), _this);
                }
                FExpression expression = visitExpression(ctx.expression());

                //as we never called visitStatement we have to manually instantiate here (important for nested lambdas)
                try { //TODO oh god this is one ugly hack
                    expression = expression.typeCheck(field.getType());
                    FStatement statement = new FExpressionStatement(expression);
                    statement = instantiateFunctionAddresses(statement);
                    assert statement instanceof FExpressionStatement;
                    expression = ((FExpressionStatement) statement).getExpression();
                } catch (UnfulfillableConstraints unfulfillableConstraints) {
                    errors.add(unfulfillableConstraints);
                    throw new Failed();
                }

                field.setAssignment(expression); //TODO field assignments need: check for cyclic dependency, register in class/object initializer etc.
                if (field.isInstance())
                    for (FParameter param : currentType.getConstructor().getParams())
                        if (param.getIdentifier().equals(field.getIdentifier()))
                            param.setDefaultValue(expression);
            } catch (Failed f) {
                //do not allow Failed to propagate any further
            } catch (IncompatibleTypes incompatibleTypes) {
                errors.add(incompatibleTypes);
            } catch (NoSuchElementException ignored) {
                //thrown of there is no constructor
            } finally {
                functionContextStack.removeLast();
            }
        }
        return field;
    }

    private void checkAccessForbidden(FTypeMember member) throws AccessForbidden {
        if (currentType != member.getMemberOf() && member.getVisibility() == FVisibilityModifier.PRIVATE) {
            throw new AccessForbidden(member);
        }
    }

    private FVariableExpression findLocal(FIdentifier identifier) throws UndeclaredVariable {
        try {
            return new FLocalVariableExpression(findLocalVar(identifier));
        } catch (UndeclaredVariable ignored) {}
        try {
            return FFieldAccess.createStatic(findStaticField(currentType, identifier));
        } catch (FieldNotFound ignored) {
        } catch (AccessForbidden accessForbidden) {
            return Utils.cantHappen();
        }
        try {
            return FFieldAccess.createInstanceTrusted(findInstanceField(currentType, identifier), getThisExpr());
        } catch (FieldNotFound | UndeclaredVariable ignored) {
        } catch (AccessForbidden accessForbidden) {
            return Utils.cantHappen();
        }
        throw new UndeclaredVariable(identifier);
    }

    private FLocalVariable findLocalVar(FIdentifier identifier) throws UndeclaredVariable {
        FLocalVariable var = currentFunction().declaredVars.get(identifier);
        if (var == null) {
            throw new UndeclaredVariable(identifier);
        }
        return var;
    }

    private FField findInstanceField(FType fClass, FIdentifier identifier) throws FieldNotFound, AccessForbidden {
        if (fClass instanceof FClass) {
            FField res = ((FClass) fClass).getInstanceFields().get(identifier);
            if (res == null)
                throw new FieldNotFound(identifier);
            checkAccessForbidden(res);
            return res;
        }
        throw new FieldNotFound(identifier);
    }

    private FField findStaticField(FType type, FIdentifier identifier) throws FieldNotFound, AccessForbidden {
        if (type instanceof FClass) {
        FField res = ((FClass) type).getStaticFields().get(identifier);
        if (res == null)
            throw new FieldNotFound(identifier);
        checkAccessForbidden(res);
        return res;
        }
        throw new FieldNotFound(identifier);
    }

    //methods enter & exitArrayAccess

    private List<FStatement> statementsFromList (List<FrontierParser.StatementContext> contexts) {
        List<FStatement> res = new ArrayList<>(contexts.size());
        for (FrontierParser.StatementContext c : contexts) {
            try {
                FStatement statement = visitStatement(c);
                if (statement instanceof FBlock && ((FBlock) statement).isEmpty())
                    continue;
                res.add(statement);
            } catch (Failed f) {
                //this is fine, failed statements are not added to the list, they will have raised errors
            }
        }
        return res;
    }

    @Override
    public FFunction visitMethodDeclaration(FrontierParser.MethodDeclarationContext ctx) {
        FFunction f = treeData.functions.get(ctx.methodHeader());
        functionContextStack.addLast(new FunctionContext(f));
        try {
            currentFunction().declaredVars.push(Utils.asMap(f.getParams()));
            ctx.methodHeader().accept(this);
            f.setBody(visitBlock(ctx.block()));
            return f;
        } finally {
            functionContextStack.removeLast();
        }
    }

    @Override
    public Object visitMethodHeader(FrontierParser.MethodHeaderContext ctx) {
        for (FrontierParser.TypeParameterSpecificationContext c : ctx.typeParameterSpecification()) {
            try {
                ParserContextUtils.handleTypeParameterSpecification(c, currentFunction().function.getParameters(), this::findType);
            } catch (SyntaxError syntaxError) {
                errors.add(syntaxError);
            }
        }
        visitFormalParameters(ctx.formalParameters());
        return null;
    }

    @Override
    public Object visitFormalParameter(FrontierParser.FormalParameterContext ctx) {
        FrontierParser.ExpressionContext c = ctx.expression();
        if (c != null) {
            try {
                treeData.parameters.get(ctx).setDefaultValue(visitExpression(c));
            } catch (IncompatibleTypes incompatibleTypes) {
                errors.add(incompatibleTypes);
            }
        }
        return null;
    }

    //statements
    public FStatement visitStatement(FrontierParser.StatementContext ctx) throws Failed {
        assert currentFunction().genericFunctionAddressToInstantiate.isEmpty();

        FStatement res = (FStatement) ctx.accept(this);

        try {
            return instantiateFunctionAddresses(res);
        } catch (UnfulfillableConstraints unfulfillableConstraints) {
            errors.add(unfulfillableConstraints);
            throw new Failed();
        }
    }

    private FStatement instantiateFunctionAddresses(FStatement untyped) throws UnfulfillableConstraints {
        if(currentFunction().genericFunctionAddressToInstantiate.isEmpty())
            return untyped;

        Multimap<FTypeVariable, TypeConstraint> newConstraints = ArrayListMultimap.create();
        for (FTypeVariable toInstantiate : currentFunction().genericFunctionAddressToInstantiate) {
            Pair<FType, Multimap<FTypeVariable, TypeConstraint>> pair = toInstantiate.getConstraints().resolve();
            assert !(pair.a instanceof FTypeVariable);
            newConstraints.putAll(pair.b);
        }
        for (Map.Entry<FTypeVariable, TypeConstraint> entry : newConstraints.entries()) {
            entry.getKey().tryAddConstraint(entry.getValue());
        }
        currentFunction().genericFunctionAddressToInstantiate.clear();
        return GenericBaking.bake(untyped);
    }

    @Override
    public FBlock visitEmptyStatement(FrontierParser.EmptyStatementContext ctx) {
        return FBlock.from();
    }

    @Override
    public FExpressionStatement visitExpressionStatement(FrontierParser.ExpressionStatementContext ctx) {
        return new FExpressionStatement(visitExpression(ctx.expression()));
    }

    @Override
    public FReturn visitReturnStatement(FrontierParser.ReturnStatementContext ctx) {
        FrontierParser.ExpressionContext c = ctx.expression();
        FExpression val = c == null ? null : visitExpression(c);
        try {
            return FReturn.create(val, currentFunction().function);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    @Override
    public FVarAssignment visitAssignment(FrontierParser.AssignmentContext ctx) {
        FVariableExpression var;
        try {
            FExpression e = visitExpression(ctx.expression(0));
            if (!(e instanceof FVariableExpression))
                throw new NonAssignableExpression(e);
            var = (FVariableExpression) e;
        } catch (NonAssignableExpression e) {
            errors.add(e);
            visitExpression(ctx.expression(1));
            throw new Failed();
        }
        FExpression value = visitExpression(ctx.expression(1));
        FVarAssignment.Operator op = FVarAssignment.Operator.fromString(ctx.getChild(1).getText());
        try {
            return FVarAssignment.create(var, op, value);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    @Override
    public FVarDeclaration visitLocalVariableDeclarationStatement(FrontierParser.LocalVariableDeclarationStatementContext ctx) {
        return visitLocalVariableDeclaration(ctx.localVariableDeclaration());
    }

    @Override
    public FVarDeclaration visitLocalVariableDeclaration(FrontierParser.LocalVariableDeclarationContext ctx) {
        FrontierParser.TypeTypeContext tc = ctx.typeType();
        FrontierParser.ExpressionContext ec = ctx.expression();

        boolean failed = false;

        //identifier
        FIdentifier identifier = ParserContextUtils.getVarIdentifier(ctx.identifier());
        if (currentFunction().declaredVars.contains(identifier)) {
            errors.add(new TwiceDefinedLocalVariable(identifier));
            failed = true;
        }

        //explicit type if present
        FType type = null;
        if (tc != null) {
            try {
                type = ParserContextUtils.getType(tc, this::findType);
            } catch (SyntaxError e) {
                errors.add(e);
                failed = true;
            }
        }

        //expression if present
        FExpression val = null;
        if (ec != null) {
            val = visitExpression(ec);

            if (type == null)
                type = val.getType();
        }

        if (type == null || type == FNull.NULL_TYPE) { //type inference failed TODO better error message in case of null fail
            errors.add(new UntypedVariable(identifier));
            failed = true;
        }

        if (failed)
            throw new Failed();

        FLocalVariable var = new FLocalVariable(identifier, type);
        FVarAssignment assign = null;
        if (val != null) {
            try {
                assign = FVarAssignment.create(new FLocalVariableExpression(var), FVarAssignment.Operator.ASSIGN, val);
            } catch (IncompatibleTypes incompatibleTypes) {
                errors.add(incompatibleTypes);
                throw new Failed();
            }
        }

        if (var.getType() == FTypeType.INSTANCE) {
            FTypeVariable typeVar = FTypeVariable.create((FTypeIdentifier) identifier, true);
            typeVariableMap.put(var, typeVar);
        }
        currentFunction().declaredVars.put(identifier, var);
        return FVarDeclaration.create(var, assign);
    }

    @Override
    public FBlock visitBlock(FrontierParser.BlockContext ctx) {
        currentFunction().declaredVars.push();
        try {
            List<FStatement> statements = statementsFromList(ctx.statement());
            if (!errors.isEmpty())
                return FBlock.from();
            for (int i = 0; i < statements.size()-1; i++) {
                FStatement statement = statements.get(i);
                if (statement.redirectsControlFlow().isPresent()) {
                    warnings.add(new UnreachableStatements(statements.subList(i+1, statements.size())));
                    statements = statements.subList(0, i+1);
                    break;
                }
            }
            return FBlock.from(statements);
        } finally {
            currentFunction().declaredVars.pop();
        }
    }

    @Override
    public FBlock visitBlockStatement(FrontierParser.BlockStatementContext ctx) {
        return visitBlock(ctx.block());
    }

    @Override
    public FIf visitIfStatement(FrontierParser.IfStatementContext ctx) {
        try {
            FExpression cond = visitExpression(ctx.expression());
            FIf res = FIf.create(cond, null, null);
            res = (FIf) instantiateFunctionAddresses(res);
            res.setThen(visitBlock(ctx.block(0)));
            if (ctx.block(1) != null) {
                res.setElse(visitBlock(ctx.block(1)));
            } else if (ctx.ifStatement() != null) {
                res.setElse(FBlock.from(visitIfStatement(ctx.ifStatement())));
            }
            return res;
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    private FLoopIdentifier startLoop() {
        FLoopIdentifier identifier = new FLoopIdentifier();
        currentFunction().loops.push(identifier);
        currentFunction().declaredVars.push();
        return identifier;
    }

    private void endLoop() {
        currentFunction().loops.pop();
        currentFunction().declaredVars.pop();
    }

    @Override
    public FWhile visitWhileStatement(FrontierParser.WhileStatementContext ctx) {
        FLoopIdentifier identifier = startLoop();
        try {
            FExpression cond = visitExpression(ctx.expression());

            FWhile res = FWhile.create(currentFunction().loops.size(), identifier, cond, null);
            res = (FWhile) instantiateFunctionAddresses(res);
            res.setBody(visitBlock(ctx.block()));
            return res;
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        } finally {
            endLoop();
        }
    }

    @Override
    public FFor visitForStatement(FrontierParser.ForStatementContext ctx) {
        FLoopIdentifier identifier = startLoop();
        try {
            FrontierParser.LocalVariableDeclarationContext dc = ctx.localVariableDeclaration();
            FVarDeclaration decl = dc == null ? null : visitLocalVariableDeclaration(dc);

            FrontierParser.ExpressionContext ec = ctx.expression();
            FExpression cond = ec == null ? null : visitExpression(ec);

            FrontierParser.Expression2Context c2 = ctx.expression2();
            FExpression inc = c2 == null ? null : visitExpression(c2.expression());

            FFor res = FFor.create(currentFunction().loops.size(), identifier, decl, cond, inc, null);
            res = (FFor) instantiateFunctionAddresses(res);
            res.setBody(visitBlock(ctx.block()));
            return res;
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        } finally {
            endLoop();
        }
    }

    @Override
    public FForEach visitForeachStatement(FrontierParser.ForeachStatementContext ctx) {
        FLoopIdentifier identifier = startLoop();
        try {
            FExpression container = visitExpression(ctx.expression());
            FVariableIdentifier id = new FVariableIdentifier(ctx.LCIdentifier().getText());
            FType varType;
            if (container.getType() instanceof FArray) {
                varType = ((FArray) container.getType()).getBaseType();
            } else {
                return Utils.NYI("non array for each");
            }
            FLocalVariable it = new FLocalVariable(id, varType);
            currentFunction().declaredVars.put(it.getIdentifier(), it);

            FForEach res = FForEach.create(currentFunction().loops.size(), identifier, it, container, null);
            res = (FForEach) instantiateFunctionAddresses(res);
            res.setBody(visitBlock(ctx.block()));
            return res;
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        } finally {
            endLoop();
        }
    }

    @Override
    public FBreak visitBreakStatement(FrontierParser.BreakStatementContext ctx) {
        if (currentFunction().loops.isEmpty()) {
            errors.add(new StatementOutsideLoop());
            throw new Failed();
        }
        return new FBreak(currentFunction().loops.peek());
    }

    @Override
    public FContinue visitContinueStatement(FrontierParser.ContinueStatementContext ctx) {
        if (currentFunction().loops.isEmpty()) {
            errors.add(new StatementOutsideLoop());
            throw new Failed();
        }
        return new FContinue(currentFunction().loops.peek());
    }


    //Expressions
    public FExpression visitExpression(FrontierParser.ExpressionContext ctx) throws Failed {
        return (FExpression) ctx.accept(this);
    }

    @Override
    public FLiteralExpression visitLiteralExpr(FrontierParser.LiteralExprContext ctx) {
        return new FLiteralExpression(visitLiteral(ctx.literal()));
    }

    private FLocalVariableExpression getThisExpr() throws UndeclaredVariable {
        return new FLocalVariableExpression(findLocalVar(FVariableIdentifier.THIS));
    }

    @Override
    public FLocalVariableExpression visitThisExpr(FrontierParser.ThisExprContext ctx) {
        try {
            return getThisExpr();
        } catch (UndeclaredVariable e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FVariableExpression visitVariableExpr(FrontierParser.VariableExprContext ctx) {
        FIdentifier identifier = ParserContextUtils.getVarIdentifier(ctx.identifier());
        try {
            return findLocal(identifier);
        } catch (UndeclaredVariable undeclaredVariable) {
            errors.add(undeclaredVariable);
            throw new Failed();
        }
    }

    @Override
    public FBracketsExpression visitBracketsExpr(FrontierParser.BracketsExprContext ctx) {
        return new FBracketsExpression(visitExpression(ctx.expression()));
    }

    @Override
    public FOptElse visitOptionalElse(FrontierParser.OptionalElseContext ctx) {
        try {
            FExpression optional = visitExpression(ctx.expression(0));
            FExpression orElse = visitExpression(ctx.expression(1));
            return FOptElse.create(optional, orElse);
        } catch (Failed f) {
            visitExpression(ctx.expression(1));
            throw f;
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitPreUnaryOp(FrontierParser.PreUnaryOpContext ctx) {
        FExpression expression = visitExpression(ctx.expression());
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.getChild(0).getText() + '_');

        if(expression.getType() instanceof FPredefinedClass &&
                (identifier.equals(FUnaryOperator.Pre.INC.identifier) || identifier.equals(FUnaryOperator.Pre.DEC.identifier))) {
            //special case for inc and dec on predefined types, they are both write and read
            if ((expression instanceof FVariableExpression)) {
                ((FVariableExpression) expression).setAccessType(FVariableExpression.AccessType.LOAD_AND_STORE);
            } else {
                errors.add(new NonAssignableExpression(expression));
                throw new Failed();
            }
        }

        try {
            return functionCall(expression.getType(), identifier, Arrays.asList(expression));
        } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitBinaryOp(FrontierParser.BinaryOpContext ctx) {
        FExpression first;
        try {
            first = visitExpression(ctx.expression(0));
        } catch (Failed f) {
            //still try to parse the second, we might find other errors
            visitExpression(ctx.expression(1));
            throw f;
        }
        FExpression second = visitExpression(ctx.expression(1));
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.getChild(1).getText());

        try {
            return functionCall(first.getType(), identifier, Arrays.asList(first, second));
        } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes e1) {
            try {
                return functionCall(second.getType(), identifier, Arrays.asList(first, second));
            } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes e2) {
                errors.add(e1);
                errors.add(e2);
                throw new Failed();
            }
        }
    }

    @Override
    public FExplicitCast visitCast(FrontierParser.CastContext ctx) {
        FExpression castedExpression = visitExpression(ctx.expression());
        FType type;
        if (ctx.EXMARK() != null) {
            if (!(castedExpression.getType() instanceof FOptional)) {
                errors.add(new NonOptionalExMark(castedExpression));
                throw new Failed();
            }
            type = ((FOptional) castedExpression.getType()).getBaseType();
        } else {
            try {
                type = ParserContextUtils.getType(ctx.typeType(), this::findType);
            } catch (SyntaxError e) {
                errors.add(e);
                throw new Failed();
            }
        }
        try {
            return FExplicitCast.create(type, castedExpression);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    @Override
    public FArrayAccess visitArrayAccess(FrontierParser.ArrayAccessContext ctx) {
        FExpression array;
        try {
            array = visitExpression(ctx.expression(0));
        } catch (Failed f) {
            //still try to parse the second, we might find other errors
            visitExpression(ctx.expression(1));
            throw f;
        }
        FExpression index = visitExpression(ctx.expression(1));
        try {
            return FArrayAccess.create(array, index);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    @Override
    public FFieldAccess visitFieldAccess(FrontierParser.FieldAccessContext ctx) {
        FIdentifier identifier = ParserContextUtils.getVarIdentifier(ctx.identifier());
        FType namespace;
        FExpression object = visitExpression(ctx.expression());
        try {
            if (object.getType() == FTypeType.INSTANCE) {
                if (object instanceof FClassExpression) {
                    namespace = ((FClassExpression) object).getfClass();
                    return FFieldAccess.createStatic(findStaticField(namespace, identifier));
                } else {
                    return Utils.NYI("a field access on an expression of Type FTypeType that is not an FClassExpression");
                }
            } else {
                return FFieldAccess.createInstance(findInstanceField(object.getType(), identifier), object);
            }
        } catch (FieldNotFound | AccessForbidden | IncompatibleTypes e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public List<FExpression> visitExpressionList(FrontierParser.ExpressionListContext ctx) {
        if (ctx == null)
            return Collections.emptyList();
        List<FrontierParser.ExpressionContext> cs = ctx.expression();
        List<FExpression> res = new ArrayList<>(cs.size());
        boolean failed = false;
        for (FrontierParser.ExpressionContext c : cs) {
            try {
                res.add(visitExpression(c));
            } catch (Failed f) {
                failed = true;
            }
        }
        if (failed)
            throw new Failed();
        return res;
    }

    private FFunctionCall functionCall (FType clazz, FFunctionIdentifier identifier, List<FExpression> params)
            throws FunctionNotFound, AccessForbidden, IncompatibleTypes {
        FFunction f = clazz.resolveFunction(identifier, Utils.typesFromExpressionList(params), null, TypeInstantiation.EMPTY);
        checkAccessForbidden(f);
        return FFunctionCall.create(f, params);
    }

    @Override
    public FExpression visitExternalFunctionCall(FrontierParser.ExternalFunctionCallContext ctx) {
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.LCIdentifier().getText());
        List<FExpression> params = new ArrayList<>();
        FType namespace;

        try {
            FExpression object = visitExpression(ctx.expression());
            if (object.getType() instanceof FFunctionType) {
                return DynamicFunctionCall.create(object, visitExpressionList(ctx.expressionList()));
            } else  if (object.getType() == FTypeType.INSTANCE) {
                if (object instanceof FClassExpression)
                    namespace = ((FClassExpression) object).getfClass();
                else
                    return Utils.NYI("a call on an expression of Type FTypeType that is not an FClassExpression");
            } else {
                namespace = object.getType();
                params.add(object);
            }
            params.addAll(visitExpressionList(ctx.expressionList()));
            return functionCall(namespace, identifier, params);
        } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionAddress visitLambdaExpr(FrontierParser.LambdaExprContext ctx) {
        FLambda lambda = visitLambda(ctx.lambda());
        currentFunction().genericFunctionAddressToInstantiate.addAll(lambda.getParametersList());
        return new FFunctionAddress(lambda);
    }

    @Override
    public FLambda visitLambda(FrontierParser.LambdaContext ctx) {
        FFunctionIdentifier id = currentType.getFreshLambdaName();
        Map<FTypeIdentifier, FTypeVariable> parameters = new HashMap<>();
        ImmutableList<FParameter> params;
        try {
            params = visitLambdaHeader(ctx.lambdaHeader(), parameters);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
        FTypeVariable returnType = FTypeVariable.create(new FTypeIdentifier("ReturnOf" + id.name), false);
        parameters.put(returnType.getIdentifier(), returnType);
        FLambda res = FLambda.create(id, currentType, returnType, params, parameters);
        currentType.addFunctionTrusted(res);

        //suspend all things of the current function (note that lambdas can appear inside lambdas, so this can go arbitrarily deep)
        functionContextStack.addLast(new FunctionContext(res));
        try {
            //push params
            currentFunction().declaredVars.push(Utils.asMap(res.getParams()));
            FBlock body;
            if (ctx.expression() != null) { //expression Lambda
                FExpression expression = visitExpression(ctx.expression());
                FStatement _return = FReturn.createTrusted(expression, res);

                //as we never called visitStatement we have to manually instantiate here (important for nested lambdas)
                try {
                    _return = instantiateFunctionAddresses(_return);
                } catch (UnfulfillableConstraints unfulfillableConstraints) {
                    errors.add(unfulfillableConstraints);
                    throw new Failed();
                }

                body = FBlock.from(_return);
            } else { //block lambda
                body = visitBlock(ctx.block());
                if(!body.redirectsControlFlow().isPresent()) {
                    returnType.tryAddConstraint(new ImplicitCastable(res, FVoid.INSTANCE, Variance.Invariant));
                }
            }
            res.setBody(body);
            res.finishLambda();
            return res;
        } finally { //restore the outside context
            assert currentFunction().genericFunctionAddressToInstantiate.isEmpty();
            functionContextStack.removeLast();
        }
    }

    public ImmutableList<FParameter> visitLambdaHeader(FrontierParser.LambdaHeaderContext ctx, Map<FTypeIdentifier, FTypeVariable> parameters) throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable {
        ImmutableList.Builder<FParameter> params = ImmutableList.builder();
        for (FrontierParser.LambdaParamContext c : ctx.lambdaParam()) {
            params.add(visitLambdaParam(c, parameters));
        }
        return params.build();
    }

    public FParameter visitLambdaParam(FrontierParser.LambdaParamContext ctx, Map<FTypeIdentifier, FTypeVariable> parameters) throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable {
        FrontierParser.IdentifierContext idContext = ctx.identifier();
        if (idContext != null) {
            FIdentifier identifier = ParserContextUtils.getVarIdentifier(idContext);
            FrontierParser.TypeTypeContext typeTypeContext = ctx.typeType();
            FType type;
            if (typeTypeContext != null) {
                type = ParserContextUtils.getType(typeTypeContext, this::findType);
            } else {
                type = FTypeVariable.create(new FTypeIdentifier("TypeOf" + identifier.name), false);
                parameters.put(type.getIdentifier(), (FTypeVariable) type);
            }
            return FParameter.create(identifier, type, false);
        } else { //Underscore
            return FParameter.create(UnnamedIdentifier.get(), FVoid.INSTANCE, false);
        }
    }

    @Override
    public FExpression visitInternalFunctionCall(FrontierParser.InternalFunctionCallContext ctx) { //TODO this needs far better resolving...
        //first check if we have a variable of function type
        FIdentifier identifier = new FVariableIdentifier(ctx.LCIdentifier().getText());
        List<FExpression> params = visitExpressionList(ctx.expressionList());
        try {
            FVariableExpression var = findLocal(identifier);
            if (var.getType() instanceof FFunctionType) {
                return DynamicFunctionCall.create(var, params);
            }
        } catch (UndeclaredVariable ignored) {
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }

        //now check for instance/static functions
        FFunctionIdentifier fIdentifier = new FFunctionIdentifier(ctx.LCIdentifier().getText());
        try {
            List<FExpression> params2 = new ArrayList<>(params.size() + 1);
            params2.add(getThisExpr());
            params2.addAll(params);
            return functionCall(currentType, fIdentifier, params2);
        } catch (FunctionNotFound | UndeclaredVariable | AccessForbidden | IncompatibleTypes e) {
            //instance method not found, check for static method
            try {
                return functionCall(currentType, fIdentifier, params);
            } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes e2) {
                errors.add(e2);
                throw new Failed();
            }
        }
    }

    @Override
    public FFunctionCall visitNewObject(FrontierParser.NewObjectContext ctx) {
        FType type;
        try {
            type = ParserContextUtils.getType(ctx.typeType(), this::findType);
        } catch (SyntaxError e) {
            errors.add(e);
            visitExpressionList(ctx.expressionList()); //parse param list to find more errors
            throw new Failed();
        }
        List<FExpression> params = visitExpressionList(ctx.expressionList());
        try {
            return functionCall(type, FConstructor.IDENTIFIER, params);
        } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitNewArray(FrontierParser.NewArrayContext ctx) {
        FExpression expression = visitExpression(ctx.expression());

        FType baseType;
        try {
            baseType = ParserContextUtils.getType(ctx.typeType(), this::findType);
        } catch (SyntaxError e) {
            errors.add(e);
            throw new Failed();
        }

        FArray array = FArray.getArrayFrom(baseType);
        try {
            return functionCall(array, FConstructor.IDENTIFIER, Arrays.asList(expression));
        } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FClassExpression visitTypeTypeExpr(FrontierParser.TypeTypeExprContext ctx) {
        try {
            FType fType = ParserContextUtils.getType(ctx.typeType(), this::findType);
            return new FClassExpression(fType);
        } catch (SyntaxError e) {
            errors.add(e);
            throw new Failed();
        }
    }

    private FFunction sthsthFunctionAddress(FFunction function) { //TODO name
        if (function.getParametersList().isEmpty())
            return function;

        FFunction res = InstantiableFunctionCopy.instantiableCopyOf(function);
        currentFunction().genericFunctionAddressToInstantiate.addAll(res.getParametersList());
        return res;
    }

    @Override
    public FFunctionAddress visitInternalFunctionAddress(FrontierParser.InternalFunctionAddressContext ctx) {
        try {
            List<FType> params = ctx.typeList() != null ? ParserContextUtils.typesFromList(ctx.typeList().typeType(), this::findType) : null;
            FFunction function = getFunction(currentType, new FFunctionIdentifier(ctx.LCIdentifier().getText()), params);
            function = sthsthFunctionAddress(function);
            return new FFunctionAddress(function);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    @Override
    public FFunctionAddress visitFunctionAddress(FrontierParser.FunctionAddressContext ctx) {
        try {
            FType fClass = ParserContextUtils.getType(ctx.typeType(), this::findType);
            List<FType> params = ctx.typeList() != null ? ParserContextUtils.typesFromList(ctx.typeList().typeType(), this::findType) : null;
            FFunction function = getFunction(fClass, new FFunctionIdentifier(ctx.LCIdentifier().getText()), params);
            function = sthsthFunctionAddress(function);
            return new FFunctionAddress(function);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    //TODO check why this is different/more complex then functionCall (should functionCall call this?, comment why not)
    private FFunction getFunction(FType fClass, FFunctionIdentifier identifier, List<FType> params) throws FunctionNotFound, AccessForbidden {
        if (!(fClass instanceof FClass))
            throw new FunctionNotFound(identifier, params);
        if (params == null) {
            Collection<FFunction> fun = ((FClass) fClass).getFunctions().get(identifier);
            if (fun.size() != 1)
                throw new FunctionNotFound(identifier, Collections.emptyList());
            return fun.iterator().next();
        } else {
            FFunction f = fClass.resolveFunction(identifier, params, null, TypeInstantiation.EMPTY);
            checkAccessForbidden(f);
            return f;
        }
    }

    //literals
    //TODO move literal processing steps from lexer to parser to properly deal with them
    //TODO store original representation as well to be able to reconstruct it
    @Override
    public FLiteral visitLiteral(FrontierParser.LiteralContext ctx) {
        return ParserContextUtils.getLiteral(ctx);
    }

    private static class Failed extends RuntimeException {}
}

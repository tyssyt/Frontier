package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.MultimapBuilder;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.*;
import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FExplicitCast;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.InstantiableFunctionCopy;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.*;
import tys.frontier.code.literal.FLambda;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.ParsedFile;
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

public class ToInternalRepresentation extends FrontierBaseVisitor<Object> {

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
    private List<Warning> warnings;
    private List<SyntaxError> errors;

    private FClass currentType;
    private Map<FTypeIdentifier, FTypeVariable> currentTypeParams;
    private Deque<FunctionContext> functionContextStack = new ArrayDeque<>();
    private Map<FVariable, FTypeVariable> typeVariableMap = new HashMap<>();

    private ParsedFile file;

    private boolean useLhsresolve = false;


    private ToInternalRepresentation(ParsedFile file, List<Warning> warnings, List<SyntaxError> errors) {
        this.treeData = file.getTreeData();
        this.file = file;
        this.warnings = warnings;
        this.errors = errors;
    }

    public static void toInternal(ParsedFile file, List<Warning> warnings, List<SyntaxError> errors) {
        ToInternalRepresentation visitor = new ToInternalRepresentation(file, warnings, errors);
        try {
            visitor.visitFile(visitor.treeData.root);
        } catch (AssertionError assertionError) {
            if (visitor.errors.isEmpty())
                throw assertionError;
        }
    }

    private FunctionContext currentFunction() {
        return functionContextStack.getLast();
    }

    private FType findType(FTypeIdentifier identifier) {
        //check type parameters of current function
        if (!functionContextStack.isEmpty() && currentFunction().function != null) {
            FType res = currentFunction().function.getParameters().get(identifier);
            if (res != null)
                return res;
        }
        //check type parameters of current class
        FType res = currentTypeParams.get(identifier);
        if (res != null)
            return res;
        //check module & imports
        res = file.resolveType(identifier);
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
                    for (FParameter param : currentType.getConstructor().getSignature().getParameters())
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
            currentFunction().declaredVars.push(Utils.asMap(f.getSignature().getParameters()));
            ctx.methodHeader().accept(this);
            FBlock body = visitBlock(ctx.block());
            if (f.getType() != FTuple.VOID && f != body.redirectsControlFlow().orElse(null) && errors.isEmpty()) { //TODO make a flag that checks if there are any errors thrown in the current method instead of errors.isEmpty
                //f should return something, but doesn't
                errors.add(new MissingReturn(f));
            }
            f.setBody(body);
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

        for (FTypeVariable toInstantiate : currentFunction().genericFunctionAddressToInstantiate) {
            toInstantiate.getConstraints().hardResolve();
        }
        FStatement res = GenericBaking.bake(untyped);
        currentFunction().genericFunctionAddressToInstantiate.clear();
        return res;
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
        FrontierParser.TupleExpressionContext c = ctx.tupleExpression();
        List<FExpression> vals = c == null ? Collections.emptyList() : visitTupleExpression(c);
        try {
            return FReturn.create(vals, currentFunction().function);
        } catch (IncompatibleTypes | TooManyArguments | NotEnoughArguments | UnfulfillableConstraints syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    @Override
    public FAssignment visitAssignment(FrontierParser.AssignmentContext ctx) {
        List<FExpression> values = visitTupleExpression(ctx.tupleExpression());
        List<FType> types = FTuple.unpackType(FTuple.fromExpressionList(values));

        List<FrontierParser.AssignLhsContext> contexts = ctx.assignLhss().assignLhs();

        if (contexts.size() > types.size()) {
            errors.add(new NotEnoughArguments("Not enough arguments in assignment", null)); //TODO can't get I type because we didn't parse it yet
            throw new Failed();
        }

        boolean failed = false;
        List<FExpression> lhsExpressions = new ArrayList<>(contexts.size());
        for (Pair<FrontierParser.AssignLhsContext, FType> pair : Utils.zip(contexts, types)) {
            try {
                lhsExpressions.add(visitAssignLhs(pair.a, pair.b));
            } catch (Failed f) {
                failed = true;
            }
        }
        if (failed)
            throw new Failed();

        try {
            return FAssignment.create(lhsExpressions, values);
        } catch (IncompatibleTypes | TooManyArguments | NotEnoughArguments | UnfulfillableConstraints incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    private FExpression visitAssignLhs(FrontierParser.AssignLhsContext ctx, FType type) {
        if (ctx.COLON() != null) {
            //declaration
            return visitAssignLhsDecl(ctx, type);
        } else {
            //assignment
            useLhsresolve = true;
            FExpression e = visitExpression(ctx.expression());
            if (useLhsresolve) {
                errors.add(new NonAssignableExpression(e));
                throw new Failed();
            }
            return e;
        }
    }

    private FVarDeclaration visitAssignLhsDecl(FrontierParser.AssignLhsContext ctx, FType type) {
        FIdentifier identifier = ParserContextUtils.getVarIdentifier(ctx.identifier());

        boolean failed = false;

        if (currentFunction().declaredVars.contains(identifier)) {
            errors.add(new TwiceDefinedLocalVariable(identifier));
            failed = true;
        }

        FrontierParser.TypeTypeContext tc = ctx.typeType();
        if (tc != null) { //explicit type
            try {
                type = ParserContextUtils.getType(tc, this::findType);
            } catch (SyntaxError e) {
                errors.add(e);
                failed = true;
            }
        }

        if (type == FNull.NULL_TYPE) { //type inference failed TODO better error message in case of null fail
            errors.add(new UntypedVariable(identifier));
            failed = true;
        }

        if (failed)
            throw new Failed();

        FLocalVariable var = new FLocalVariable(identifier, type);
        if (var.getType() == FTypeType.INSTANCE) {
            FTypeVariable typeVar = FTypeVariable.create((FTypeIdentifier) identifier, true);
            typeVariableMap.put(var, typeVar);
        }
        currentFunction().declaredVars.put(identifier, var);
        return new FVarDeclaration(var);
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
    public FForEach visitForeachStatement(FrontierParser.ForeachStatementContext ctx) {
        FLoopIdentifier identifier = startLoop();
        try {
            FExpression container = visitExpression(ctx.expression());
            List<FVariableIdentifier> ids = new ArrayList<>();
            boolean failed = false;
            for (TerminalNode node : ctx.LCIdentifier()) {
                FVariableIdentifier id = new FVariableIdentifier(node.getText());
                if (currentFunction().declaredVars.contains(id)) {
                    errors.add(new TwiceDefinedLocalVariable(id));
                    failed = true;
                }
                ids.add(id);
            }

            List<FType> types;
            if (container.getType() instanceof FArray) {
                types = FTuple.unpackType(((FArray) container.getType()).getBaseType());
            } else {
                return Utils.NYI("non array for each");
            }

            if (ids.size() < types.size() || ids.size() > types.size()+1) {
                errors.add(new WrongNumberOfIdentifiersInFor(ids, types));
                failed = true;
            }

            if (failed)
                throw new Failed();

            List<FLocalVariable> vars = new ArrayList<>(ids.size());
            for (Pair<FVariableIdentifier, FType> pair : Utils.zip(ids, types)) {
                FLocalVariable var = new FLocalVariable(pair.a, pair.b);
                vars.add(var);
                currentFunction().declaredVars.put(var.getIdentifier(), var);
            }

            FLocalVariable counter = null;
            if (ids.size() == types.size()+1) {
                counter = new FLocalVariable(ids.get(ids.size()-1), FIntN._32); //TODO int32 vs int64 (arrays need int32, custom data types might need int64 or more)
                currentFunction().declaredVars.put(counter.getIdentifier(), counter);
            }

            FForEach res = FForEach.create(currentFunction().loops.size(), identifier, vars, counter, container, null);
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
        boolean lhsResolve = useLhsresolve; //TODO use when field access uses functions
        useLhsresolve = false;
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
                (identifier.equals(UnaryOperator.INC.identifier) || identifier.equals(UnaryOperator.DEC.identifier))) {
            //special case for inc and dec on predefined types, they are both write and read
            if ((expression instanceof FVariableExpression)) {
                ((FVariableExpression) expression).setAccessType(FVariableExpression.AccessType.LOAD_AND_STORE);
            } else {
                errors.add(new NonAssignableExpression(expression));
                throw new Failed();
            }
        }

        try {
            return functionCall(expression.getType(), identifier, Arrays.asList(expression), ImmutableListMultimap.of(), false);
        } catch (FunctionNotFound | AccessForbidden e) {
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
            return functionCall(first.getType(), identifier, Arrays.asList(first, second), ImmutableListMultimap.of(), false);
        } catch (FunctionNotFound | AccessForbidden e1) {
            try {
                return functionCall(second.getType(), identifier, Arrays.asList(first, second), ImmutableListMultimap.of(), false);
            } catch (FunctionNotFound | AccessForbidden e2) {
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
            if (castedExpression instanceof FTuple)
                Utils.NYI("! is undefined on tuples (kind of)"); //TODO
            if (!(castedExpression.getType() instanceof FOptional)) { //TODO this should be changed to FOptional.canBeTreatedAsOptional, but I need to decide on the semantics of using ! on optional tuples
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
        boolean lhsResolve = useLhsresolve; //TODO use when this is an operator
        useLhsresolve = false;
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
        boolean lhsResolve = useLhsresolve; //TODO use when field access uses functions
        useLhsresolve = false;
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
    public Pair<List<FExpression>, ListMultimap<FIdentifier, FExpression>> visitArguments(FrontierParser.ArgumentsContext ctx) {
        if (ctx == null)
            return new Pair<>(Collections.emptyList(), ImmutableListMultimap.of());
        return new Pair<>(visitTupleExpression(ctx.tupleExpression()), visitNamedExpressions(ctx.namedExpressions()));
    }

    @Override
    public List<FExpression> visitTupleExpression(FrontierParser.TupleExpressionContext ctx) {
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

    @Override
    public ListMultimap<FIdentifier, FExpression> visitNamedExpressions(FrontierParser.NamedExpressionsContext ctx) { //TODO this doesn't follow the fail paradigm
        if (ctx == null)
            return ImmutableListMultimap.of();
        ListMultimap<FIdentifier, FExpression> res = MultimapBuilder.linkedHashKeys().arrayListValues().build();
        for (FrontierParser.NamedExpressionContext c : ctx.namedExpression()) {
            res.putAll(ParserContextUtils.getVarIdentifier(c.identifier()), visitTupleExpression(c.tupleExpression()));
        }
        return res;
    }

    private FFunctionCall functionCall (FType clazz, FFunctionIdentifier identifier, List<FExpression> positionalArgs, ListMultimap<FIdentifier, FExpression> keywordArgs, boolean lhsResolve)
            throws FunctionNotFound, AccessForbidden {
        FunctionResolver.Result res = clazz.hardResolveFunction(identifier, Utils.typesFromExpressionList(positionalArgs), Utils.typesFromExpressionMap(keywordArgs), null, lhsResolve);
        checkAccessForbidden(res.getFunction());
        return FFunctionCall.create(res.signature, positionalArgs, keywordArgs, res.argMapping);
    }

    @Override
    public FExpression visitExternalFunctionCall(FrontierParser.ExternalFunctionCallContext ctx) {
        boolean lhsResolve = useLhsresolve;
        useLhsresolve = false;
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.LCIdentifier().getText());
        FType namespace;

        try {
            FExpression object = visitExpression(ctx.expression());
            Pair<List<FExpression>, ListMultimap<FIdentifier, FExpression>> arguments = visitArguments(ctx.arguments());
            if (object.getType() instanceof FFunctionType) {
                if (!arguments.b.isEmpty())
                    throw new DynamicCallWithKeywordArgs(object, arguments.b);
                return DynamicFunctionCall.create(object, arguments.a);
            } else  if (object.getType() == FTypeType.INSTANCE) {
                if (object instanceof FClassExpression)
                    namespace = ((FClassExpression) object).getfClass();
                else
                    return Utils.NYI("a call on an expression of Type FTypeType that is not an FClassExpression");
            } else {
                namespace = object.getType();
                if (arguments.a.isEmpty())
                    arguments.a = Collections.singletonList(object);
                else
                    arguments.a.add(0, object);
            }
            return functionCall(namespace, identifier, arguments.a, arguments.b, lhsResolve);
        } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes | DynamicCallWithKeywordArgs e) {
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
            currentFunction().declaredVars.push(Utils.asMap(res.getSignature().getParameters()));
            FBlock body;
            if (ctx.expression() != null) { //expression Lambda
                FExpression expression = visitExpression(ctx.expression());
                FStatement _return;
                if (expression.getType() == FTuple.VOID) {
                    _return = new FExpressionStatement(expression);
                    returnType.tryAddConstraint(new ImplicitCastable(_return, FTuple.VOID, Variance.Invariant));
                } else
                    _return = FReturn.createTrusted(Arrays.asList(expression), res);

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
                    returnType.tryAddConstraint(new ImplicitCastable(res, FTuple.VOID, Variance.Invariant));
                }
            }
            res.setBody(body);
            res.finishLambda();
            return res;
        } finally { //restore the outside context
            assert currentFunction().genericFunctionAddressToInstantiate.isEmpty() || !errors.isEmpty();
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
            return FParameter.create(UnnamedIdentifier.get(), FTuple.VOID, false);
        }
    }

    @Override
    public FExpression visitInternalFunctionCall(FrontierParser.InternalFunctionCallContext ctx) { //TODO this needs far better resolving...
        boolean lhsResolve = useLhsresolve;
        useLhsresolve = false;
        //first check if we have a variable of function type
        FIdentifier identifier = new FVariableIdentifier(ctx.LCIdentifier().getText());
        Pair<List<FExpression>, ListMultimap<FIdentifier, FExpression>> arguments = visitArguments(ctx.arguments());
        try {
            FVariableExpression var = findLocal(identifier);
            if (var.getType() instanceof FFunctionType) {
                if (!arguments.b.isEmpty())
                    throw new DynamicCallWithKeywordArgs(var, arguments.b);
                return DynamicFunctionCall.create(var, arguments.a);
            }
        } catch (UndeclaredVariable ignored) {
        } catch (IncompatibleTypes | DynamicCallWithKeywordArgs e) {
            errors.add(e);
            throw new Failed();
        }

        //now check for instance/static functions
        FFunctionIdentifier fIdentifier = new FFunctionIdentifier(ctx.LCIdentifier().getText());
        try {
            List<FExpression> params2 = new ArrayList<>(arguments.a.size() + 1);
            params2.add(getThisExpr());
            params2.addAll(arguments.a);
            return functionCall(currentType, fIdentifier, params2, arguments.b, lhsResolve);
        } catch (FunctionNotFound | UndeclaredVariable | AccessForbidden e) {
            //instance method not found, check for static method
            try {
                return functionCall(currentType, fIdentifier, arguments.a, arguments.b, lhsResolve);
            } catch (FunctionNotFound | AccessForbidden e2) {
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
            visitNamedExpressions(ctx.namedExpressions()); //parse param list to find more errors
            throw new Failed();
        }
        ListMultimap<FIdentifier, FExpression> namedArguments = visitNamedExpressions(ctx.namedExpressions());
        try {
            return functionCall(type, FConstructor.IDENTIFIER, Collections.emptyList(), namedArguments, false);
        } catch (FunctionNotFound | AccessForbidden e) {
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
            return functionCall(array, FConstructor.IDENTIFIER, Arrays.asList(expression), ImmutableListMultimap.of(), false);
        } catch (FunctionNotFound | AccessForbidden e) {
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
            List<FType> params = ctx.typeList() != null ? FTuple.unpackType(ParserContextUtils.tupleFromList(ctx.typeList(), this::findType)) : null;
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
            List<FType> params = ctx.typeList() != null ? FTuple.unpackType(ParserContextUtils.tupleFromList(ctx.typeList(), this::findType)) : null;
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
            throw new FunctionNotFound(identifier, params, ImmutableListMultimap.of());
        if (params == null) {
            Collection<Signature> fun = ((FClass) fClass).getFunctions(false).get(identifier);  //TODO lhsResolve
            if (fun.size() != 1)
                throw new FunctionNotFound(identifier, Collections.emptyList(), ImmutableListMultimap.of());
            return fun.iterator().next().getFunction();
        } else {
            FFunction f = fClass.hardResolveFunction(identifier, params, ImmutableListMultimap.of(), null, false).getFunction();  //TODO lhsResolve
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

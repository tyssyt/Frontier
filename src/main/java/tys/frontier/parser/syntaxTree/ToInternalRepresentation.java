package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.MultimapBuilder;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.*;
import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FExplicitCast;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.InstantiableFunctionCopy;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.Operator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.UnnamedIdentifier;
import tys.frontier.code.literal.*;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.statement.loop.forImpl.ForByIdx;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.statement.loop.forImpl.ForPlaceholder;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.ParsedFile;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.parser.warnings.UnreachableStatements;
import tys.frontier.parser.warnings.Warning;
import tys.frontier.passes.GenericBaking;
import tys.frontier.util.MapStack;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static tys.frontier.util.Utils.mutableSingletonList;

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

    private FClass currenClass;
    private DefaultNamespace currentNamespace;
    private Map<FIdentifier, FTypeVariable> currentTypeParams;
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

    private Namespace findNamespace(FIdentifier identifier, boolean searchLocal) throws TypeNotFound {
        //check type parameters of current function
        if (!functionContextStack.isEmpty() && currentFunction().function != null) {
            FType res = currentFunction().function.getParameters().get(identifier);
            if (res != null)
                return res.getNamespace();
        }
        //check type parameters of current class
        if (currentTypeParams != null) {
            FType type = currentTypeParams.get(identifier);
            if (type != null)
                return type.getNamespace();
        }
        //check module & imports
        Namespace namespace = file.resolveNamespace(identifier);
        if (namespace != null)
            return namespace;
        if (searchLocal) {
            //check for declaration of type variables
            try {
                FExpression local = findLocal(null, identifier, false);
                if (local instanceof FVariableExpression) {
                    FVariableExpression varExpression = (FVariableExpression) local;
                    return typeVariableMap.get(varExpression.getVariable()).getNamespace();
                } else
                    return Utils.NYI("resolving to a Type that is stored in a field of Type Type");
            } catch (UndeclaredVariable undeclaredVariable) {
                throw new TypeNotFound(identifier);
            }
        }
        throw new TypeNotFound(identifier);
    }

    private Namespace findNamespaceNoThrow(FIdentifier identifier) {
        try {
            return findNamespace(identifier, true);
        } catch (TypeNotFound ignored) {
            return null;
        }
    }

    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentNamespace = treeData.classNamespaces.get(ctx);
        currenClass = currentNamespace.getType();
        //noinspection unchecked
        currentTypeParams = Utils.asTypeMap((List<FTypeVariable>) currenClass.getParametersList());
        try {
            //handle typeParameterSpecification
            for (FrontierParser.TypeParameterSpecificationContext c : ctx.typeParameterSpecification()) {
                try {
                    ParserContextUtils.handleTypeParameterSpecification(c, currentTypeParams, this::findNamespaceNoThrow);
                } catch (SyntaxError syntaxError) {
                    errors.add(syntaxError);
                }
            }
            return visitChildren(ctx);
        } finally {
            currentNamespace = null;
            currenClass = null;
            currentTypeParams = null;
        }
    }

    @Override
    public Object visitNamespaceDeclaration(FrontierParser.NamespaceDeclarationContext ctx) {
        currentNamespace = treeData.namespaces.get(ctx);
        try {
            return visitChildren(ctx);
        } finally {
            currentNamespace = null;
        }
    }

    @Override
    public Object visitForDeclarative(FrontierParser.ForDeclarativeContext ctx) {
        assert currenClass.getForImpl() == ForPlaceholder.INSTANCE;
        functionContextStack.addLast(new FunctionContext(null));
        try {
            currentFunction().declaredVars.push(); //TODO is this needed?

            FExpression exp1 = visitExpression(ctx.expression(0));
            exp1 = exp1.typeCheck(FFunctionType.from(
                    FTuple.from(currenClass, FIntN._32),
                    FTypeVariable.create(new FIdentifier("ElementType"), false))); //identifier doesn't matter, just picked one that existed...
            exp1 = instantiateFunctionAddresses(exp1);
            if (exp1 instanceof FImplicitCast)
                exp1 = ((FImplicitCast) exp1).getCastedExpression();
            if (!(exp1 instanceof FFunctionAddress))
                throw new InvalidForDeclaration("for declaration needs Lambda or Function Address", exp1);
            FFunction getElement = ((FFunctionAddress) exp1).getFunction();

            FExpression exp2 = visitExpression(ctx.expression(1));
            exp2 = exp2.typeCheck(FFunctionType.from(currenClass, FIntN._32));
            exp2 = instantiateFunctionAddresses(exp2);
            if (exp2 instanceof FImplicitCast)
                exp2 = ((FImplicitCast) exp2).getCastedExpression();
            if (!(exp2 instanceof FFunctionAddress))
                throw new InvalidForDeclaration("for declaration needs Lambda or Function Address", exp1);
            FFunction getSize = ((FFunctionAddress) exp2).getFunction();

            ForByIdx forImpl = new ForByIdx(getElement, getSize);
            currenClass.setForImpl(forImpl);
            return forImpl;
        } catch (Failed f) {
            return null;
        } catch (IncompatibleTypes | UnfulfillableConstraints | InvalidForDeclaration e) {
            errors.add(e);
            return null;
        } finally {
            functionContextStack.removeLast();
        }
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
                try {
                    expression = expression.typeCheck(field.getType());
                    expression = instantiateFunctionAddresses(expression);
                } catch (UnfulfillableConstraints unfulfillableConstraints) {
                    errors.add(unfulfillableConstraints);
                    throw new Failed();
                }

                field.setAssignment(expression); //TODO field assignments need: check for cyclic dependency, register in class/object initializer etc.
                if (field.isInstance()) {
                    ImmutableList<FParameter> parameters = currenClass.getConstructor().getSignature().getParameters();
                    for (FParameter param : parameters)
                        if (param.getIdentifier().equals(field.getIdentifier())) {
                            param.setDefaultValue(expression, ParserContextUtils.findDefaultValueDependencies(expression, parameters));
                        }
                }
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

    private void checkAccessForbidden(FFunction fFunction) throws AccessForbidden {
        if (currentNamespace != fFunction.getMemberOf() && fFunction.getVisibility() == FVisibilityModifier.PRIVATE) {
            throw new AccessForbidden(fFunction);
        }
    }

    private FExpression findLocal(Position position, FIdentifier identifier, boolean lhsResolve) throws UndeclaredVariable {
        try {
            return new FVariableExpression(position, findLocalVar(identifier));
        } catch (UndeclaredVariable ignored) {}
        try {
            return functionCall(position, currentNamespace, identifier, singletonList(getThisExpr(null)), ImmutableListMultimap.of(), lhsResolve);
        } catch (AccessForbidden accessForbidden) {
            return Utils.cantHappen();
        } catch (UndeclaredVariable | FunctionNotFound ignored) {}
        try {
            return functionCall(position, currentNamespace, identifier, emptyList(), ImmutableListMultimap.of(), lhsResolve);
        } catch (AccessForbidden accessForbidden) {
            return Utils.cantHappen();
        } catch (FunctionNotFound ignored) {}
        try {
            return new FNamespaceExpression(position, findNamespace(identifier, false));
        } catch (TypeNotFound ignored) {}
        throw new UndeclaredVariable(identifier);
    }

    private FLocalVariable findLocalVar(FIdentifier identifier) throws UndeclaredVariable {
        FLocalVariable var = currentFunction().declaredVars.get(identifier);
        if (var == null) {
            throw new UndeclaredVariable(identifier);
        }
        return var;
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
    public Object visitNativeMethodDeclaration(FrontierParser.NativeMethodDeclarationContext ctx) {
        FFunction f = treeData.functions.get(ctx.methodHeader());
        functionContextStack.addLast(new FunctionContext(f));
        try {
            currentFunction().declaredVars.push(Utils.asMap(f.getSignature().getParameters()));
            ctx.methodHeader().accept(this);
            return f;
        } finally {
            functionContextStack.removeLast();
        }
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
        if (ctx.typeType() != null) {
            try {
                validateRemoteFunction();
            } catch (SyntaxError e) {
                errors.add(e);
            }
        } else if (currenClass != null) {
            //overloads of open functions follow more strict requirements, even if not remote
            FFunction open = currenClass.getNamespace().getOpen(currentFunction().function.getIdentifier());
            if (open != null) {
                try {
                    validateOpenOverload(open, false);
                } catch (InvalidSignatureOpenOverload invalidSignatureOpenOverload) {
                    errors.add(invalidSignatureOpenOverload);
                }
            }
        }

        for (FrontierParser.TypeParameterSpecificationContext c : ctx.typeParameterSpecification()) {
            try {
                ParserContextUtils.handleTypeParameterSpecification(c, currentFunction().function.getParameters(), this::findNamespaceNoThrow);
            } catch (SyntaxError syntaxError) {
                errors.add(syntaxError);
            }
        }
        visitFormalParameters(ctx.formalParameters());
        return null;
    }

    private void validateRemoteFunction() throws NonOpenRemoteFunctionDeclaration, InvalidSignatureOpenOverload {
        //the function was pushed into a different namespace, make sure that was legal
        Namespace remoteNamespace = currentFunction().function.getMemberOf();
        FFunction open = remoteNamespace.getOpen(currentFunction().function.getIdentifier());
        if (open == null)
            throw new NonOpenRemoteFunctionDeclaration(currentFunction().function, "remote does not declare signature as open");

        validateOpenOverload(open, true);
    }

    private void validateOpenOverload(FFunction open, boolean remote) throws InvalidSignatureOpenOverload {
        List<FTypeVariable> openParameters = open.getParametersList();
        for (Pair<FParameter, FParameter> pair : Utils.zip(open.getSignature().getParameters(), currentFunction().function.getSignature().getParameters())) {
            if (pair.a.getType() == pair.b.getType())
                continue;
            //noinspection SuspiciousMethodCalls
            if (!(pair.a.getType() instanceof FTypeVariable) || !openParameters.contains(pair.a.getType()))
                throw new InvalidSignatureOpenOverload(currentFunction().function, open);
            if (remote && pair.b.getType() != currenClass)
                throw new InvalidSignatureOpenOverload(currentFunction().function, open);
        }
    }

    @Override
    public Object visitFormalParameter(FrontierParser.FormalParameterContext ctx) {
        FrontierParser.ExpressionContext c = ctx.expression();
        if (c != null) {
            try {
                FExpression defaultValue = visitExpression(c);
                ImmutableList<FParameter> parameters = currentFunction().function.getSignature().getParameters();
                treeData.parameters.get(ctx).setDefaultValue(defaultValue, ParserContextUtils.findDefaultValueDependencies(defaultValue, parameters));
            } catch (IncompatibleTypes incompatibleTypes) {
                errors.add(incompatibleTypes);
            } catch (Failed ignored) {}
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

    //TODO allow expression baking to avoid the wrapper expression
    private FExpression instantiateFunctionAddresses(FExpression untyped) throws UnfulfillableConstraints {
        FStatement statement = new FExpressionStatement(null, untyped);
        statement = instantiateFunctionAddresses(statement);
        assert statement instanceof FExpressionStatement;
        return  ((FExpressionStatement) statement).getExpression();
    }

    private FStatement instantiateFunctionAddresses(FStatement untyped) throws UnfulfillableConstraints {
        if(currentFunction().genericFunctionAddressToInstantiate.isEmpty())
            return untyped;

        for (FTypeVariable toInstantiate : currentFunction().genericFunctionAddressToInstantiate) {
            toInstantiate.hardResolve();
        }
        FStatement res = GenericBaking.bake(untyped);
        currentFunction().genericFunctionAddressToInstantiate.clear();
        return res;
    }

    @Override
    public FBlock visitEmptyStatement(FrontierParser.EmptyStatementContext ctx) {
        return FBlock.from(Position.fromCtx(ctx));
    }

    @Override
    public FExpressionStatement visitExpressionStatement(FrontierParser.ExpressionStatementContext ctx) {
        return new FExpressionStatement(Position.fromCtx(ctx), visitExpression(ctx.expression()));
    }

    @Override
    public FReturn visitReturnStatement(FrontierParser.ReturnStatementContext ctx) {
        FrontierParser.TupleExpressionContext c = ctx.tupleExpression();
        List<FExpression> vals = c == null ? emptyList() : visitTupleExpression(c);
        try {
            return FReturn.create(Position.fromCtx(ctx), vals, currentFunction().function);
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
            return FAssignment.create(Position.fromCtx(ctx), lhsExpressions, values);
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
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());

        boolean failed = false;

        if (currentFunction().declaredVars.contains(identifier)) {
            errors.add(new TwiceDefinedLocalVariable(identifier));
            failed = true;
        }

        FrontierParser.TypeTypeContext tc = ctx.typeType();
        if (tc != null) { //explicit type
            try {
                type = ParserContextUtils.getType(tc, this::findNamespaceNoThrow);
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
            FTypeVariable typeVar = FTypeVariable.create(identifier, true);
            typeVariableMap.put(var, typeVar);
        }
        currentFunction().declaredVars.put(identifier, var);
        return new FVarDeclaration(Position.fromCtx(ctx), var);
    }

    @Override
    public FBlock visitBlock(FrontierParser.BlockContext ctx) {
        currentFunction().declaredVars.push();
        try {
            return FBlock.from(Position.fromCtx(ctx), createBlock(ctx.statement()));
        } finally {
            currentFunction().declaredVars.pop();
        }
    }

    private List<FStatement> createBlock(List<FrontierParser.StatementContext> statement1) {
        List<FStatement> statements = statementsFromList(statement1);
        if (!errors.isEmpty())
            statements = emptyList();
        for (int i = 0; i < statements.size()-1; i++) {
            FStatement statement = statements.get(i);
            if (statement.redirectsControlFlow().isPresent()) {
                warnings.add(new UnreachableStatements(statements.subList(i+1, statements.size())));
                statements = statements.subList(0, i+1);
                break;
            }
        }
        return statements;
    }

    public FBlock visitLamdaBlock(FrontierParser.LamdaBlockContext ctx, List<FType> types) throws SyntaxError {
        FrontierParser.LambdaHeaderContext lambdaHeaderContext = ctx.lambdaHeader();
        if (lambdaHeaderContext == null)
            return FBlock.from(Position.fromCtx(ctx), createBlock(ctx.statement()));

        List<FLocalVariable> lambdaVars = visitLambdaHeader(lambdaHeaderContext, types);
        currentFunction().declaredVars.putAll(Utils.asMap(lambdaVars));
        return FLambdaBlock.from(Position.fromCtx(ctx), createBlock(ctx.statement()), lambdaVars);
    }

    @Override
    public FBlock visitBlockStatement(FrontierParser.BlockStatementContext ctx) {
        return visitBlock(ctx.block());
    }

    @Override
    public FStatement visitIfStatement(FrontierParser.IfStatementContext ctx) {
        try {
            FExpression cond = visitExpression(ctx.expression());
            FIf res = FIf.create(Position.fromCtx(ctx), cond, null, null);
            res = (FIf) instantiateFunctionAddresses(res);

            OptionalInformationForIf info = OptionalInformationForIf.createFromCondition(res.getCondition());
            res.setThen(handleIfBranch(ctx.lamdaBlock(), info));

            if (ctx.block() != null)
                res.setElse(visitBlock(ctx.block()));
            else if (ctx.ifStatement() != null) {
                res.setElse(FBlock.from(visitIfStatement(ctx.ifStatement())));
                Token elseToken = ctx.ELSE().getSymbol();
                res.cutPositionElif(elseToken.getLine(), elseToken.getCharPositionInLine() + elseToken.getText().length());
            }

            if (res.getThen().redirectsControlFlow().isEmpty() && res.getElse().flatMap(FBlock::redirectsControlFlow).isPresent()) {
                FAssignment promotions = info.createPromotions(currentFunction().declaredVars.peek());
                return FBlock.from(res.getPosition(), res, promotions);
            } else
                return res;
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    private FBlock handleIfBranch(FrontierParser.LamdaBlockContext ctx, OptionalInformationForIf info) throws SyntaxError {
        currentFunction().declaredVars.push();
        try {
            if (info.getPromotableVars().isEmpty())
                return visitLamdaBlock(ctx, info.getPromotedLambdaValueTypes());

            FAssignment promotion = info.createPromotions(currentFunction().declaredVars.peek());
            FBlock block = visitLamdaBlock(ctx, info.getPromotedLambdaValueTypes());
            return FBlock.from(block.getPosition(), promotion, block);
        } finally {
            currentFunction().declaredVars.pop();
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

            FWhile res = FWhile.create(Position.fromCtx(ctx), currentFunction().loops.size(), identifier, cond, null);
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
            List<FIdentifier> ids = new ArrayList<>();
            boolean failed = false;
            for (TerminalNode node : ctx.IDENTIFIER()) {
                FIdentifier id = new FIdentifier(node.getText());
                if (currentFunction().declaredVars.contains(id)) {
                    errors.add(new TwiceDefinedLocalVariable(id));
                    failed = true;
                }
                ids.add(id);
            }

            ForImpl forImpl = container.getType().getForImpl();
            if (forImpl == null) {
                throw new TypeDoesNotImplementFor(container);
            }
            List<FType> types = FTuple.unpackType(forImpl.getElementType());

            if (ids.size() < types.size() || ids.size() > types.size()+1) {
                errors.add(new WrongNumberOfIdentifiersInFor(ids, types));
                failed = true;
            }

            if (failed)
                throw new Failed();

            List<FLocalVariable> vars = new ArrayList<>(ids.size());
            for (Pair<FIdentifier, FType> pair : Utils.zip(ids, types)) {
                FLocalVariable var = new FLocalVariable(pair.a, pair.b);
                vars.add(var);
                currentFunction().declaredVars.put(var.getIdentifier(), var);
            }

            FLocalVariable counter = null;
            if (ids.size() == types.size()+1) {
                counter = new FLocalVariable(ids.get(ids.size()-1), FIntN._32); //TODO int32 vs int64 (arrays need int32, custom data types might need int64 or more)
                currentFunction().declaredVars.put(counter.getIdentifier(), counter);
            }

            FForEach res = FForEach.create(Position.fromCtx(ctx), currentFunction().loops.size(), identifier, vars, counter, container, null);
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
        return new FBreak(Position.fromCtx(ctx), currentFunction().loops.peek());
    }

    @Override
    public FContinue visitContinueStatement(FrontierParser.ContinueStatementContext ctx) {
        if (currentFunction().loops.isEmpty()) {
            errors.add(new StatementOutsideLoop());
            throw new Failed();
        }
        return new FContinue(Position.fromCtx(ctx), currentFunction().loops.peek());
    }


    //Expressions
    public FExpression visitExpression(FrontierParser.ExpressionContext ctx) throws Failed {
        return (FExpression) ctx.accept(this);
    }

    @Override
    public FLiteralExpression visitLiteralExpr(FrontierParser.LiteralExprContext ctx) {
        return new FLiteralExpression(Position.fromCtx(ctx), visitLiteral(ctx.literal()));
    }

    private FVariableExpression getThisExpr(Position position) throws UndeclaredVariable {
        return new FVariableExpression(position, findLocalVar(FIdentifier.THIS));
    }

    @Override
    public FVariableExpression visitThisExpr(FrontierParser.ThisExprContext ctx) {
        try {
            return getThisExpr(Position.fromCtx(ctx));
        } catch (UndeclaredVariable e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FExpression visitVariableExpr(FrontierParser.VariableExprContext ctx) {
        boolean lhsResolve = useLhsresolve;
        useLhsresolve = false;
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        try {
            return findLocal(Position.fromCtx(ctx), identifier, lhsResolve);
        } catch (UndeclaredVariable undeclaredVariable) {
            errors.add(undeclaredVariable);
            throw new Failed();
        }
    }

    @Override
    public FBracketsExpression visitBracketsExpr(FrontierParser.BracketsExprContext ctx) {
        return new FBracketsExpression(Position.fromCtx(ctx), visitExpression(ctx.expression()));
    }

    @Override
    public FOptElse visitOptionalElse(FrontierParser.OptionalElseContext ctx) {
        try {
            FExpression optional = visitExpression(ctx.expression(0));
            FExpression orElse = visitExpression(ctx.expression(1));
            return FOptElse.create(Position.fromCtx(ctx), optional, orElse);
        } catch (Failed f) {
            visitExpression(ctx.expression(1));
            throw f;
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    @Override
    public FExpression visitPreUnaryOp(FrontierParser.PreUnaryOpContext ctx) {
        if (useLhsresolve)
            return Utils.NYI("unary op lhs resolve");

        FExpression expression = visitExpression(ctx.expression());
        FIdentifier identifier = new FIdentifier(ctx.getChild(0).getText() + '_');

        if (expression instanceof FLiteralExpression && identifier.equals(UnaryOperator.NEG.identifier)) {
            FLiteral literal = ((FLiteralExpression) expression).getLiteral();
            if (literal instanceof FIntNLiteral) {
                FIntNLiteral intN = (FIntNLiteral) literal;
                return new FLiteralExpression(Position.fromCtx(ctx), new FIntNLiteral(intN.value.negate(), intN.getType(), '-' + intN.originalString));
            }
            if (literal instanceof FFloat32Literal) {
                FFloat32Literal float32 = (FFloat32Literal) literal;
                return new FLiteralExpression(Position.fromCtx(ctx), new FFloat32Literal(-float32.value, '-' + float32.originalString));
            }
            if (literal instanceof FFloat64Literal) {
                FFloat64Literal float64 = (FFloat64Literal) literal;
                return new FLiteralExpression(Position.fromCtx(ctx), new FFloat64Literal(-float64.value, '-' + float64.originalString));
            }
        }

        try {
            return functionCall(Position.fromCtx(ctx), expression.getType().getNamespace(), identifier, mutableSingletonList(expression), ImmutableListMultimap.of(), false);
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
        FIdentifier identifier = new FIdentifier(ctx.getChild(1).getText());

        try {
            return functionCall(Position.fromCtx(ctx), BinaryOperator.sGetNamespace(), identifier, asList(first, second), ImmutableListMultimap.of(), false);
        } catch (FunctionNotFound | AccessForbidden syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    @Override
    public FExplicitCast visitCast(FrontierParser.CastContext ctx) {
        FExpression castedExpression = visitExpression(ctx.expression());
        FType type;
        if (ctx.EXMARK() != null) {
            if (!(castedExpression.getType() instanceof FOptional)) { //TODO this should be changed to FOptional.canBeTreatedAsOptional, but I need to decide on the semantics of using ! on optional tuples
                errors.add(new NonOptionalExMark(castedExpression));
                throw new Failed();
            }
            type = ((FOptional) castedExpression.getType()).getBaseType();
        } else {
            try {
                type = ParserContextUtils.getType(ctx.typeType(), this::findNamespaceNoThrow);
            } catch (SyntaxError e) {
                errors.add(e);
                throw new Failed();
            }
        }
        try {
            return FExplicitCast.create(Position.fromCtx(ctx), type, castedExpression);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitArrayAccess(FrontierParser.ArrayAccessContext ctx) {
        boolean lhsResolve = useLhsresolve;
        useLhsresolve = false;
        FExpression array;
        try {
            array = visitExpression(ctx.expression());
        } catch (Failed f) {
            //still try to parse the second, we might find other errors
            visitArguments(ctx.arguments());
            throw f;
        }
        Pair<List<FExpression>, ListMultimap<FIdentifier, FExpression>> arguments = visitArguments(ctx.arguments());
        arguments.a.add(0, array);
        try {
            return functionCall(Position.fromCtx(ctx), array.getType().getNamespace(), Access.ID, arguments.a, arguments.b, lhsResolve);
        } catch (FunctionNotFound | AccessForbidden error) {
            errors.add(error);
            throw new Failed();
        }
    }

    @Override
    public Pair<List<FExpression>, ListMultimap<FIdentifier, FExpression>> visitArguments(FrontierParser.ArgumentsContext ctx) {
        if (ctx == null)
            return new Pair<>(emptyList(), ImmutableListMultimap.of());
        return new Pair<>(visitTupleExpression(ctx.tupleExpression()), visitNamedExpressions(ctx.namedExpressions()));
    }

    @Override
    public List<FExpression> visitTupleExpression(FrontierParser.TupleExpressionContext ctx) {
        if (ctx == null)
            return emptyList();
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
            res.putAll(new FIdentifier(c.IDENTIFIER().getText()), visitTupleExpression(c.tupleExpression()));
        }
        return res;
    }

    private FFunctionCall functionCall (Position position, Namespace clazz, FIdentifier identifier, List<FExpression> positionalArgs, ListMultimap<FIdentifier, FExpression> keywordArgs, boolean lhsResolve)
            throws FunctionNotFound, AccessForbidden {
        FunctionResolver.Result res = clazz.hardResolveFunction(identifier, Utils.typesFromExpressionList(positionalArgs), Utils.typesFromExpressionMap(keywordArgs), null, lhsResolve);
        checkAccessForbidden(res.getFunction());
        return FFunctionCall.create(position, res.signature, positionalArgs, keywordArgs, res.argMapping);
    }

    @Override
    public FExpression visitExternalFunctionCall(FrontierParser.ExternalFunctionCallContext ctx) {
        boolean lhsResolve = useLhsresolve;
        useLhsresolve = false;
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        Namespace namespace;

        try {
            FExpression object = visitExpression(ctx.expression());
            Pair<List<FExpression>, ListMultimap<FIdentifier, FExpression>> arguments = visitArguments(ctx.arguments());
            if (object.getType() instanceof FFunctionType) {
                if (!arguments.b.isEmpty())
                    throw new DynamicCallWithKeywordArgs(object, arguments.b);
                return DynamicFunctionCall.create(Position.fromCtx(ctx), object, arguments.a);
            } else  if (object instanceof FNamespaceExpression) {
                namespace = ((FNamespaceExpression) object).getNamespace();
            } else {
                namespace = object.getType().getNamespace();
                if (arguments.a.isEmpty())
                    arguments.a = singletonList(object);
                else
                    arguments.a.add(0, object);
            }
            return functionCall(Position.fromCtx(ctx), namespace, identifier, arguments.a, arguments.b, lhsResolve);
        } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes | DynamicCallWithKeywordArgs e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionAddress visitLambdaExpr(FrontierParser.LambdaExprContext ctx) {
        FLambda lambda = visitLambda(ctx.lambda());
        currentFunction().genericFunctionAddressToInstantiate.addAll(lambda.getParametersList());
        return new FFunctionAddress(Position.fromCtx(ctx), lambda);
    }

    @Override
    public FLambda visitLambda(FrontierParser.LambdaContext ctx) {
        FIdentifier id = currentNamespace.getFreshLambdaName();
        Map<FIdentifier, FTypeVariable> parameters = new HashMap<>();
        ImmutableList<FParameter> params;
        try {
            params = visitLambdaHeader(ctx.lambdaHeader(), parameters);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
        FTypeVariable returnType = FTypeVariable.create(new FIdentifier("ReturnOf" + id.name), false);
        parameters.put(returnType.getIdentifier(), returnType);
        Location location = new Location(currentNamespace.getLocation().getFile(), Position.fromCtx(ctx));
        FLambda res = FLambda.create(location, id, currentNamespace, returnType, params, parameters);
        currentNamespace.addFunctionTrusted(res);

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
                    _return = new FExpressionStatement(expression.getPosition(), expression);
                    returnType.tryAddConstraint(new ImplicitCastable(_return, FTuple.VOID, Variance.Invariant));
                } else
                    _return = FReturn.createTrusted(expression.getPosition(), expression, res);

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
                if(body.redirectsControlFlow().isEmpty()) {
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

    public ImmutableList<FParameter> visitLambdaHeader(FrontierParser.LambdaHeaderContext ctx, Map<FIdentifier, FTypeVariable> parameters) throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable {
        ImmutableList.Builder<FParameter> params = ImmutableList.builder();
        for (FrontierParser.LambdaParamContext c : ctx.lambdaParam()) {
            params.add(visitLambdaParam(c, parameters));
        }
        return params.build();
    }

    public FParameter visitLambdaParam(FrontierParser.LambdaParamContext ctx, Map<FIdentifier, FTypeVariable> parameters) throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable {
        TerminalNode idContext = ctx.IDENTIFIER();
        if (idContext != null) {
            FrontierParser.TypeTypeContext typeTypeContext = ctx.typeType();
            FType type;
            if (typeTypeContext != null) {
                type = ParserContextUtils.getType(typeTypeContext, this::findNamespaceNoThrow);
            } else {
                type = FTypeVariable.create(new FIdentifier("TypeOf" + new FIdentifier(idContext.getText()).name), false);
                parameters.put(type.getIdentifier(), (FTypeVariable) type);
            }
            return FParameter.create(new FIdentifier(idContext.getText()), type, false);
        } else { //Underscore
            return FParameter.create(UnnamedIdentifier.get(), FTuple.VOID, false);
        }
    }

    public List<FLocalVariable> visitLambdaHeader(FrontierParser.LambdaHeaderContext ctx, List<FType> expectedTypes) throws SyntaxError {
        List<FLocalVariable> res = new ArrayList<>();
        List<FrontierParser.LambdaParamContext> lambdaParamContexts = ctx.lambdaParam();
        if (lambdaParamContexts.size() != expectedTypes.size())
            throw new WrongNumberOfIdentifiersInFor(null, expectedTypes); //TODO not really the right error, but I stopped caring...

        for (Pair<FrontierParser.LambdaParamContext, FType> pair : Utils.zip(lambdaParamContexts, expectedTypes))
            visitLambdaParam(pair.a, pair.b).ifPresent(res::add);
        return res;
    }

    public Optional<FLocalVariable> visitLambdaParam(FrontierParser.LambdaParamContext ctx, FType type) throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable, IncompatibleTypes {
        TerminalNode idContext = ctx.IDENTIFIER();
        if (idContext != null) {
            FrontierParser.TypeTypeContext typeTypeContext = ctx.typeType();
            if (typeTypeContext != null) {
                if (type != ParserContextUtils.getType(typeTypeContext, this::findNamespaceNoThrow))
                    throw new IncompatibleTypes(ParserContextUtils.getType(typeTypeContext, this::findNamespaceNoThrow), type);
            }
            return Optional.of(new FLocalVariable(new FIdentifier(idContext.getText()), type));
        } else //Underscore
            return Optional.empty();
    }

    @Override
    public FExpression visitInternalFunctionCall(FrontierParser.InternalFunctionCallContext ctx) { //TODO this needs far better resolving...
        boolean lhsResolve = useLhsresolve;
        useLhsresolve = false;
        //first check if we have a variable of function type
        TerminalNode identifierNode = ctx.IDENTIFIER();
        FIdentifier identifier = new FIdentifier(identifierNode.getText());
        Position position = Position.fromCtx(ctx);
        Pair<List<FExpression>, ListMultimap<FIdentifier, FExpression>> arguments = visitArguments(ctx.arguments());
        try {
            FExpression var = findLocal(Position.fromToken(identifierNode.getSymbol()), identifier, lhsResolve);
            if (var.getType() instanceof FFunctionType) {
                if (!arguments.b.isEmpty())
                    throw new DynamicCallWithKeywordArgs(var, arguments.b);
                return DynamicFunctionCall.create(position, var, arguments.a);
            }
        } catch (UndeclaredVariable ignored) {
        } catch (IncompatibleTypes | DynamicCallWithKeywordArgs e) {
            errors.add(e);
            throw new Failed();
        }

        //now check for instance/static functions
        try {
            List<FExpression> params2 = new ArrayList<>(arguments.a.size() + 1);
            //TODO @PositionForGeneratedCode
            params2.add(getThisExpr(null));
            params2.addAll(arguments.a);
            return functionCall(position, currentNamespace, identifier, params2, arguments.b, lhsResolve);
        } catch (FunctionNotFound | UndeclaredVariable | AccessForbidden e) {
            //instance method not found, check for static method
            try {
                return functionCall(position, currentNamespace, identifier, arguments.a, arguments.b, lhsResolve);
            } catch (FunctionNotFound | AccessForbidden e2) {
                errors.add(e2);
                throw new Failed();
            }
        }
    }

    @Override
    public FFunctionCall visitNewObject(FrontierParser.NewObjectContext ctx) {
        Namespace namespace;
        try {
            namespace = ParserContextUtils.getNamespace(ctx.typeType(), this::findNamespaceNoThrow);
        } catch (SyntaxError e) {
            errors.add(e);
            visitNamedExpressions(ctx.namedExpressions()); //parse param list to find more errors
            throw new Failed();
        }
        ListMultimap<FIdentifier, FExpression> namedArguments = visitNamedExpressions(ctx.namedExpressions());
        try {
            return functionCall(Position.fromCtx(ctx), namespace, FConstructor.IDENTIFIER, emptyList(), namedArguments, false);
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
            baseType = ParserContextUtils.getType(ctx.typeOrTuple(), this::findNamespaceNoThrow);
        } catch (SyntaxError e) {
            errors.add(e);
            throw new Failed();
        }

        Namespace namespace = FArray.getArrayFrom(baseType).getNamespace();
        try {
            return functionCall(Position.fromCtx(ctx), namespace, FConstructor.IDENTIFIER, mutableSingletonList(expression), ImmutableListMultimap.of(), false);
        } catch (FunctionNotFound | AccessForbidden e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FNamespaceExpression visitTypeTypeExpr(FrontierParser.TypeTypeExprContext ctx) {
        try {
            Namespace namespace = ParserContextUtils.getNamespace(ctx.typeType(), this::findNamespaceNoThrow);
            return new FNamespaceExpression(Position.fromCtx(ctx), namespace);
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
            List<FType> params = ctx.typeList() != null ? FTuple.unpackType(ParserContextUtils.tupleFromList(ctx.typeList(), this::findNamespaceNoThrow)) : null;
            FIdentifier identifier;
            DefaultNamespace namespace;
            if (ctx.IDENTIFIER() != null) {
                identifier = new FIdentifier(ctx.IDENTIFIER().getText());
                namespace = currentNamespace;
            } else {
                Operator operator = Operator.get(ctx.operator().getText(), params);
                identifier = operator.getIdentifier();
                namespace = operator.getNamespace().orElse(currentNamespace);
            }
            FFunction function = getFunction(namespace, identifier, params);
            function = sthsthFunctionAddress(function);
            return new FFunctionAddress(Position.fromCtx(ctx), function);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    @Override
    public FFunctionAddress visitFunctionAddress(FrontierParser.FunctionAddressContext ctx) {
        try {
            Namespace namespace = ParserContextUtils.getNamespace(ctx.typeType(), this::findNamespaceNoThrow);
            List<FType> params = ctx.typeList() != null ? FTuple.unpackType(ParserContextUtils.tupleFromList(ctx.typeList(), this::findNamespaceNoThrow)) : null;
            FIdentifier identifier;
            if (ctx.IDENTIFIER() != null)
                identifier = new FIdentifier(ctx.IDENTIFIER().getText());
            else {
                Operator operator = Operator.get(ctx.operator().getText(), params);
                identifier = operator.getIdentifier();
                if (operator.getNamespace().isPresent())
                    namespace = operator.getNamespace().get();
            }
            FFunction function = getFunction(namespace, identifier, params);
            function = sthsthFunctionAddress(function);
            return new FFunctionAddress(Position.fromCtx(ctx), function);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    //TODO check why this is different/more complex then functionCall (should functionCall call this?, comment why not)
    private FFunction getFunction(Namespace namespace, FIdentifier identifier, List<FType> params) throws FunctionNotFound, AccessForbidden {
        if (!(namespace instanceof DefaultNamespace))
            throw new FunctionNotFound(identifier, params, ImmutableListMultimap.of());
        if (params == null) {
            Collection<Signature> fun = ((DefaultNamespace) namespace).getFunctions(false).get(identifier);  //TODO lhsResolve
            if (fun.size() != 1)
                throw new FunctionNotFound(identifier, emptyList(), ImmutableListMultimap.of());
            return fun.iterator().next().getFunction();
        } else {
            try {
                FFunction f = namespace.hardResolveFunction(identifier, params, ImmutableListMultimap.of(), null, false).getFunction();  //TODO lhsResolve
                checkAccessForbidden(f);
                return f;
            } catch (FunctionNotFound fnf) {
                if (currenClass == null)
                    throw fnf;
                params.add(0, currenClass); //static failed, try instance
                FFunction f = namespace.hardResolveFunction(identifier, params, ImmutableListMultimap.of(), null, false).getFunction();  //TODO lhsResolve
                checkAccessForbidden(f);
                return f;
            }
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

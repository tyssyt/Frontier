package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.*;
import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.function.*;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.Operator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.functionResolve.FunctionResolver;
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
import tys.frontier.code.type.FBaseClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.Constraints;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.parser.warnings.UnreachableStatements;
import tys.frontier.parser.warnings.Warning;
import tys.frontier.util.MapStack;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.math.BigInteger;
import java.util.*;

import static com.google.common.collect.Iterables.getOnlyElement;
import static java.util.Arrays.asList;
import static java.util.Collections.*;
import static java.util.stream.Collectors.toList;
import static tys.frontier.parser.antlr.FrontierParser.ArrayLiteralContext;
import static tys.frontier.parser.antlr.FrontierParser.TypeOrTupleContext;
import static tys.frontier.util.Utils.*;

public class ToInternalRepresentation extends FrontierBaseVisitor<Object> {

    private static class FunctionContext {
        FFunction function;
        Deque<FLoopIdentifier> loops = new ArrayDeque<>();
        MapStack<FIdentifier, FLocalVariable> declaredVars = new MapStack<>();
        List<FrontierParser.LambdaContext> unparsedLambdas = new ArrayList<>();

        boolean delayReturnCheck = false;
        List<FReturn> returnsToCheck;

        FunctionContext(FFunction function) {
            this.function = function;
        }
    }

    private SyntaxTreeData treeData;
    private Map<FIdentifier, DefaultNamespace> namespaceResolver;
    private List<Warning> warnings;
    private List<SyntaxError> errors;

    private FBaseClass currentClass;
    private DefaultNamespace currentNamespace;
    private Map<FIdentifier, FTypeVariable> currentTypeParams;
    private Deque<FunctionContext> functionContextStack = new ArrayDeque<>();
    private Map<FVariable, FTypeVariable> typeVariableMap = new HashMap<>();

    private boolean useLhsresolve = false;


    private ToInternalRepresentation(SyntaxTreeData treeData, Map<FIdentifier, DefaultNamespace> namespaceResolver, List<Warning> warnings, List<SyntaxError> errors) {
        this.treeData = treeData;
        this.namespaceResolver = namespaceResolver;
        this.warnings = warnings;
        this.errors = errors;
    }

    public static void toInternal(SyntaxTreeData treeData, Map<FIdentifier, DefaultNamespace> namespaceResolver, List<Warning> warnings, List<SyntaxError> errors) {
        ToInternalRepresentation visitor = new ToInternalRepresentation(treeData, namespaceResolver, warnings, errors);
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

    private void endCurrentFunction() {
        FunctionContext context = functionContextStack.removeLast();
        assert context.unparsedLambdas.isEmpty();
    }

    private Namespace findNamespace(Position position, FIdentifier identifier, boolean searchLocal) throws TypeNotFound {
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
        DefaultNamespace namespace = namespaceResolver.get(identifier);
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
                throw new TypeNotFound(position, identifier);
            }
        }

        throw new TypeNotFound(position, identifier);
    }

    private Namespace findNamespaceNoThrow(FIdentifier identifier) {
        try {
            return findNamespace(null, identifier, true);
        } catch (TypeNotFound ignored) {
            return null;
        }
    }

    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentNamespace = treeData.classNamespaces.get(ctx);
        currentClass = (FBaseClass) currentNamespace.getType();
        currentTypeParams = asTypeMap(currentClass.getParametersList());
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
            currentClass = null;
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
        assert currentClass.getForImpl() == ForPlaceholder.INSTANCE;
        functionContextStack.addLast(new FunctionContext(null));
        try {
            currentFunction().declaredVars.push(); //TODO is this needed?

            FExpression exp1 = visitExpression(ctx.expression(0));
            if (!(exp1 instanceof FFunctionAddress))
                throw new InvalidForDeclaration(Position.fromCtx(ctx), "for declaration needs Lambda or Function Address", exp1);
            FFunctionType typeGetElement = ((FFunctionAddress) exp1).getType();

            exp1 = exp1.typeCheck(FFunctionType.from(FTuple.from(currentClass, FIntN._32), typeGetElement.getOut()));
            if (exp1 instanceof FImplicitCast)
                exp1 = ((FImplicitCast) exp1).getCastedExpression();
            if (!(exp1 instanceof FFunctionAddress))
                throw new InvalidForDeclaration(Position.fromCtx(ctx), "for declaration needs Lambda or Function Address", exp1);
            FFunction getElement = ((FFunctionAddress) exp1).getFunction();

            FExpression exp2 = visitExpression(ctx.expression(1));
            exp2 = exp2.typeCheck(FFunctionType.from(currentClass, FIntN._32));
            if (exp2 instanceof FImplicitCast)
                exp2 = ((FImplicitCast) exp2).getCastedExpression();
            if (!(exp2 instanceof FFunctionAddress))
                throw new InvalidForDeclaration(Position.fromCtx(ctx), "for declaration needs Lambda or Function Address", exp2);
            FFunction getSize = ((FFunctionAddress) exp2).getFunction();

            ForByIdx forImpl = new ForByIdx(getElement, getSize);
            currentClass.setForImpl(forImpl);
            return forImpl;
        } catch (Failed f) {
            return null;
        } catch (IncompatibleTypes | InvalidForDeclaration e) {
            errors.add(e);
            return null;
        } finally {
            endCurrentFunction();
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
                if (field instanceof InstanceField) {
                    FLocalVariable _this = ((InstanceField) field).getThis();
                    currentFunction().declaredVars.put(_this.getIdentifier(), _this);
                }
                FExpression expression = visitExpression(ctx.expression());
                expression = expression.typeCheck(field.getType());
                field.setAssignment(expression);
                if (field instanceof InstanceField) {
                    ImmutableList<FParameter> parameters = currentClass.getConstructor().getSignature().getParameters();
                    for (FParameter param : parameters)
                        if (param.getIdentifier().equals(field.getIdentifier()))
                            param.setDefaultValue(expression, ParserContextUtils.findDefaultValueDependencies(expression, parameters));
                }
            } catch (Failed f) {
                //do not allow Failed to propagate any further
            } catch (IncompatibleTypes incompatibleTypes) {
                errors.add(incompatibleTypes);
            } catch (NoSuchElementException ignored) {
                //thrown of there is no constructor
            } finally {
                endCurrentFunction();
            }
        }
        return field;
    }

    private void checkAccessForbidden(Position position, FFunction fFunction) throws AccessForbidden {
        if (currentNamespace != fFunction.getMemberOf() && fFunction.getVisibility() == FVisibilityModifier.PRIVATE) {
            throw new AccessForbidden(position, fFunction);
        }
    }

    private FExpression findLocal(Position position, FIdentifier identifier, boolean lhsResolve) throws UndeclaredVariable {
        try {
            return new FVariableExpression(position, findLocalVar(position, identifier));
        } catch (UndeclaredVariable ignored) {}
        try {
            return functionCall(position, currentNamespace, identifier, singletonList(getThisExpr(null)), emptyMap(), lhsResolve);
        } catch (AccessForbidden accessForbidden) {
            return Utils.cantHappen();
        } catch (UndeclaredVariable | FunctionNotFound | IncompatibleTypes ignored) {}
        try {
            return functionCall(position, currentNamespace, identifier, emptyList(), emptyMap(), lhsResolve);
        } catch (AccessForbidden accessForbidden) {
            return Utils.cantHappen();
        } catch (FunctionNotFound | IncompatibleTypes ignored) {}
        try {
            return new FNamespaceExpression(position, findNamespace(position, identifier, false));
        } catch (TypeNotFound ignored) {}
        throw new UndeclaredVariable(position, identifier);
    }

    private FLocalVariable findLocalVar(Position position, FIdentifier identifier) throws UndeclaredVariable {
        FLocalVariable var = currentFunction().declaredVars.get(identifier);
        if (var == null) {
            throw new UndeclaredVariable(position, identifier);
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
            endCurrentFunction();
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
            endCurrentFunction();
        }
    }

    @Override
    public Object visitMethodHeader(FrontierParser.MethodHeaderContext ctx) {
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
        return (FStatement) ctx.accept(this);
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
        Position position = Position.fromCtx(ctx);

        FExpression expression = null;
        FrontierParser.ExpressionListContext expressionsCtx = ctx.expressionList();
        if (expressionsCtx != null) {
            List<FExpression> expressions = visitExpressionList(expressionsCtx);
            if (expressions.size() == 1)
                expression = getOnlyElement(expressions);
            else
                expression = new Pack(position, expressions);
        }

        if (currentFunction().delayReturnCheck) {
            FReturn _return = FReturn.createWithDelayedCheck(position, expression, currentFunction().function);
            currentFunction().returnsToCheck.add(_return);
            return _return;
        }

        try {
            return FReturn.create(position, expression, currentFunction().function);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    @Override
    public FAssignment visitAssignment(FrontierParser.AssignmentContext ctx) {
        List<FExpression> values = visitExpressionList(ctx.expressionList());
        List<FrontierParser.AssignLhsContext> lhss = ctx.assignLhss().assignLhs();

        boolean implicitUnpack = false;
        List<FType> rhsTypes;
        if (values.size() == 1 && lhss.size() > 1) {
            // implicit unpack
            implicitUnpack = true;
            rhsTypes = FTuple.unpackType(getOnlyElement(values).getType());
        } else if (lhss.size() == 1 && values.size() > 1) {
            // implicit pack
            values = mutableSingletonList(new Pack(Position.fromCtx(ctx.expressionList()), values));
            rhsTypes = List.of(getOnlyElement(values).getType());
        } else {
            rhsTypes = typesFromExpressionList(values);
        }

        if (rhsTypes.size() > lhss.size()) {
            errors.add(new TooManyArguments(values.get(lhss.size()).getType()));
            throw new Failed();
        } else if (lhss.size() > rhsTypes.size()) {
            errors.add(new NotEnoughArguments(Position.fromCtx(ctx), "Not enough arguments in assignment", null)); //TODO can't get I type because we didn't parse it yet
            throw new Failed();
        }

        boolean failed = false;
        List<FExpression> lhsExpressions = new ArrayList<>(lhss.size());
        for (Pair<FrontierParser.AssignLhsContext, FType> pair : zip(lhss, rhsTypes)) {
            try {
                lhsExpressions.add(visitAssignLhs(pair.a, pair.b));
            } catch (Failed f) {
                failed = true;
            } catch (SyntaxError e) {
                errors.add(e);
                failed = true;
            }
        }
        if (failed)
            throw new Failed();

        if (implicitUnpack) {
            lhsExpressions = List.of(new Pack(Position.fromCtx(ctx.assignLhss()), lhsExpressions));
        }

        try {
            return FAssignment.create(Position.fromCtx(ctx), lhsExpressions, values);
        } catch (IncompatibleTypes | TooManyArguments | NotEnoughArguments | UnfulfillableConstraints incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    private FExpression visitAssignLhs(FrontierParser.AssignLhsContext ctx, FType type) throws SyntaxError {
        List<FrontierParser.SimpleLhsContext> cs = ctx.simpleLhs();

        if (cs.isEmpty()) { // function call
            useLhsresolve = true;
            FExpression e = visitExpression(ctx.expression());
            if (useLhsresolve)
                throw new NonAssignableExpression(e);
            return e;
        }

        if (cs.size() == 1)
            return visitSimpleLhs(getOnlyElement(cs), type);

        // tuple deconstruction
        List<FType> types = FTuple.unpackType(type);
        if (types.size() > cs.size())
            throw new TooManyArguments(types.get(cs.size()));
        else if (cs.size() > types.size())
            throw new NotEnoughArguments(Position.fromCtx(ctx), "Not enough arguments in assignment", null); //TODO can't get I type because we didn't parse it yet

        List<FExpression> expressions = new ArrayList<>(cs.size());
        for (Pair<FrontierParser.SimpleLhsContext, FType> pair : zip(cs, types))
            expressions.add(visitSimpleLhs(pair.a, pair.b));
        return new Pack(Position.fromCtx(ctx), expressions);
    }

    private FExpression visitSimpleLhs(FrontierParser.SimpleLhsContext ctx, FType type) throws SyntaxError {
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        Position pos = Position.fromCtx(ctx);

        if (ctx.COLON() == null) { // assignment
            return findLocal(pos, identifier, true);
        } else { // declaration
            //TODO forbid type inference for unbound expression
            if (currentFunction().declaredVars.contains(identifier))
                throw new TwiceDefinedLocalVariable(pos, currentFunction().declaredVars.get(identifier).getPosition(), identifier);

            // handle explicit types
            FrontierParser.TypeTypeContext tc = ctx.typeType();
            if (tc != null)
                type = ParserContextUtils.getType(tc, this::findNamespaceNoThrow);

            if (type == FNull.NULL_TYPE)
                throw new UntypedVariable(pos, identifier); //type inference failed TODO better error message in case of null fail

            FLocalVariable var = new FLocalVariable(pos, identifier, type);
            if (var.getType() == FTypeType.INSTANCE)
                typeVariableMap.put(var, FTypeVariable.create(new Location(currentFunction().function.getLocation().getFile(), pos), identifier));

            currentFunction().declaredVars.put(identifier, var);
            return new FVarDeclaration(var);
        }
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

    private List<FLocalVariable> visitLambdaHeader(FrontierParser.LambdaHeaderContext ctx, List<FType> expectedTypes) throws SyntaxError {
        List<FLocalVariable> res = new ArrayList<>();
        List<FrontierParser.LambdaParamContext> lambdaParamContexts = ctx.lambdaParam();
        if (lambdaParamContexts.size() != expectedTypes.size())
            throw new WrongNumberOfIdentifiersInFor(Position.fromCtx(ctx), null, expectedTypes); //TODO not really the right error, but I stopped caring...

        for (Pair<FrontierParser.LambdaParamContext, FType> pair : zip(lambdaParamContexts, expectedTypes))
            visitLambdaParam(pair.a, pair.b).ifPresent(res::add);
        return res;
    }

    private Optional<FLocalVariable> visitLambdaParam(FrontierParser.LambdaParamContext ctx, FType type) throws WrongNumberOfTypeArguments, TypeNotFound, ParameterizedTypeVariable, IncompatibleTypes, NonEmbeddableType {
        TerminalNode idContext = ctx.IDENTIFIER();
        if (idContext != null) {
            FrontierParser.TypeTypeContext typeTypeContext = ctx.typeType();
            if (typeTypeContext != null) {
                if (type != ParserContextUtils.getType(typeTypeContext, this::findNamespaceNoThrow))
                    throw new IncompatibleTypes(ParserContextUtils.getType(typeTypeContext, this::findNamespaceNoThrow), type);
            }
            return Optional.of(new FLocalVariable(Position.fromCtx(ctx), new FIdentifier(idContext.getText()), type));
        } else //Underscore
            return Optional.empty();
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

            OptionalInformationForIf info = OptionalInformationForIf.createFromCondition(res.getCondition());
            res.setThen(handleThenBranch(ctx.lamdaBlock(), info));
            handleElseBranch(ctx, res, info);
            return maybePostPromote(res, info);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    private FBlock handleThenBranch(FrontierParser.LamdaBlockContext ctx, OptionalInformationForIf info) throws SyntaxError {
        currentFunction().declaredVars.push();
        try {
            if (info.isPromoteThen() && !info.getPromotableVars().isEmpty()) {
                FAssignment promotion = info.createPromotions(currentFunction().declaredVars.peek());
                FBlock block = visitLamdaBlock(ctx, info.getPromotedLambdaValueTypes());
                return FBlock.from(block.getPosition(), promotion, block);
            } else
                return visitLamdaBlock(ctx, info.getPromotedLambdaValueTypes());
        } finally {
            currentFunction().declaredVars.pop();
        }
    }

    //TODO I don't like this, there has to be a more elegant solution
    private void handleElseBranch(FrontierParser.IfStatementContext ctx, FIf res, OptionalInformationForIf info) {
        currentFunction().declaredVars.push();
        try {
            FAssignment promotion = null;
            if (!info.isPromoteThen() && !info.getPromotableVars().isEmpty()) {
                promotion = info.createPromotions(currentFunction().declaredVars.peek());
            }
            if (ctx.block() != null) {
                FBlock block = visitBlock(ctx.block());
                FBlock elseBody = promotion != null ? FBlock.from(block.getPosition(), promotion, block) : block;
                res.setElse(elseBody);
            } else if (ctx.ifStatement() != null) {
                FStatement statement = visitIfStatement(ctx.ifStatement());
                FBlock elseBody = promotion != null ? FBlock.from(statement.getPosition(), promotion, statement) : FBlock.from(statement);
                res.setElse(elseBody);
                res.cutPositionElif(ctx.ELSE().getSymbol());
            }
        } finally {
            currentFunction().declaredVars.pop();
        }
    }

    private FStatement maybePostPromote(FIf res, OptionalInformationForIf info) {
        if (!info.getPromotableVars().isEmpty()) {
            boolean thenRedirects = res.getThen().redirectsControlFlow().isPresent();
            boolean elseRedirects = res.getElse().flatMap(FBlock::redirectsControlFlow).isPresent();
            if (( info.isPromoteThen() && !thenRedirects &&  elseRedirects) ||
                    (!info.isPromoteThen() &&  thenRedirects && !elseRedirects)) {
                FAssignment promotions = info.createPromotions(currentFunction().declaredVars.peek());
                return FBlock.from(res.getPosition(), res, promotions);
            }
        }
        return res;
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
            Position pos = Position.fromCtx(ctx);
            for (TerminalNode node : ctx.IDENTIFIER()) {
                FIdentifier id = new FIdentifier(node.getText());
                if (currentFunction().declaredVars.contains(id)) {
                    errors.add(new TwiceDefinedLocalVariable(pos, currentFunction().declaredVars.get(id).getPosition(), id));
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
                errors.add(new WrongNumberOfIdentifiersInFor(pos, ids, types));
                failed = true;
            }

            if (failed)
                throw new Failed();

            List<FLocalVariable> vars = new ArrayList<>(ids.size());
            for (Pair<FIdentifier, FType> pair : zip(ids, types)) {
                FLocalVariable var = new FLocalVariable(pos, pair.a, pair.b);
                vars.add(var);
                currentFunction().declaredVars.put(var.getIdentifier(), var);
            }

            FLocalVariable counter = null;
            if (ids.size() == types.size()+1) {
                counter = new FLocalVariable(pos, ids.get(ids.size()-1), FIntN._32); //TODO int32 vs int64 (arrays need int32, custom data types might need int64 or more)
                currentFunction().declaredVars.put(counter.getIdentifier(), counter);
            }

            FForEach res = FForEach.create(pos, currentFunction().loops.size(), identifier, vars, counter, container, null);
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
        Position pos = Position.fromCtx(ctx);
        if (currentFunction().loops.isEmpty()) {
            errors.add(new StatementOutsideLoop(pos));
            throw new Failed();
        }
        return new FBreak(pos, currentFunction().loops.peek());
    }

    @Override
    public FContinue visitContinueStatement(FrontierParser.ContinueStatementContext ctx) {
        Position pos = Position.fromCtx(ctx);
        if (currentFunction().loops.isEmpty()) {
            errors.add(new StatementOutsideLoop(pos));
            throw new Failed();
        }
        return new FContinue(pos, currentFunction().loops.peek());
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
        return new FVariableExpression(position, findLocalVar(position, FIdentifier.THIS));
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
    public Object visitTupleExpression(FrontierParser.TupleExpressionContext ctx) {
        List<FExpression> expressions = visitExpressionList(ctx.expressionList());
        if (expressions.size() == 1)
            return getOnlyElement(expressions);
        else
            return new Pack(Position.fromCtx(ctx), expressions);
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
                return new FLiteralExpression(Position.fromCtx(ctx), new FIntNLiteral(intN.value.negate()));
            }
            if (literal instanceof FFloat32Literal) {
                FFloat32Literal float32 = (FFloat32Literal) literal;
                return new FLiteralExpression(Position.fromCtx(ctx), new FFloat32Literal(-float32.value));
            }
            if (literal instanceof FFloat64Literal) {
                FFloat64Literal float64 = (FFloat64Literal) literal;
                return new FLiteralExpression(Position.fromCtx(ctx), new FFloat64Literal(-float64.value));
            }
        }

        try {
            return functionCall(Position.fromCtx(ctx), expression.getType().getNamespace(), identifier, mutableSingletonList(expression), emptyMap(), false);
        } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FExpression visitBinaryOp(FrontierParser.BinaryOpContext ctx) {
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

        if (first instanceof FLiteralExpression && second instanceof FLiteralExpression) {
            FLiteralExpression res = getBinaryLiteralExpression(ctx, ((FLiteralExpression) first).getLiteral(), ((FLiteralExpression) second).getLiteral());
            if (res != null)
                return res;
        }


        Position position = Position.fromCtx(ctx);
        try {
                return functionCall(position, first.getType().getNamespace(), identifier, asList(first, second), emptyMap(), false);
        } catch (SyntaxError e1) {
            try {
                // TODO not all multiplications are commutative, so this can't be done in all cases!
                return functionCall(position, second.getType().getNamespace(), identifier, asList(second, first), emptyMap(), false);
            } catch (SyntaxError e2) {
                errors.add(e1);
                throw new Failed();
            }
        }
    }

    private FLiteralExpression getBinaryLiteralExpression(FrontierParser.BinaryOpContext ctx, FLiteral first, FLiteral second) {
        if (first instanceof FIntNLiteral && second instanceof FIntNLiteral) {
            BigInteger firstInt = ((FIntNLiteral) first).value;
            BigInteger secondInt = ((FIntNLiteral) second).value;
            FLiteral newLiteral = switch (BinaryOperator.getFromParserToken(ctx.getChild(1).getText())) {
                case PLUS    -> new FIntNLiteral(firstInt.add(secondInt));
                case MINUS   -> new FIntNLiteral(firstInt.subtract(secondInt));
                case TIMES   -> new FIntNLiteral(firstInt.multiply(secondInt));
                case DIVIDED -> new FIntNLiteral(firstInt.divide(secondInt));
                case MODULO  -> new FIntNLiteral(firstInt.mod(secondInt));
                case AAND    -> new FIntNLiteral(firstInt.and(secondInt));
                case AOR     -> new FIntNLiteral(firstInt.or(secondInt));
                case XOR     -> new FIntNLiteral(firstInt.xor(secondInt));
                case EQUALS, EQUALS_ID         -> firstInt.equals(secondInt) ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case NOT_EQUALS, NOT_EQUALS_ID -> firstInt.equals(secondInt) ? FBoolLiteral.FALSE : FBoolLiteral.TRUE;
                case LESS          -> firstInt.compareTo(secondInt) <  0 ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case GREATER       -> firstInt.compareTo(secondInt) >  0 ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case LESS_EQUAL    -> firstInt.compareTo(secondInt) <= 0 ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case GREATER_EQUAL -> firstInt.compareTo(secondInt) >= 0 ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                default -> null;
            };
            if (newLiteral != null)
                return new FLiteralExpression(Position.fromCtx(ctx), newLiteral);
        }

        if (first instanceof FFloat32Literal && second instanceof FFloat32Literal) {
            float firstFloat = ((FFloat32Literal) first).value;
            float secondFloat = ((FFloat32Literal) second).value;
            FLiteral newLiteral = switch (BinaryOperator.getFromParserToken(ctx.getChild(1).getText())) {
                case PLUS    -> new FFloat32Literal(firstFloat + secondFloat);
                case MINUS   -> new FFloat32Literal(firstFloat - secondFloat);
                case TIMES   -> new FFloat32Literal(firstFloat * secondFloat);
                case DIVIDED -> new FFloat32Literal(firstFloat / secondFloat);
                case MODULO  -> new FFloat32Literal(firstFloat % secondFloat);
                case EQUALS, EQUALS_ID         -> firstFloat == secondFloat ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case NOT_EQUALS, NOT_EQUALS_ID -> firstFloat != secondFloat ? FBoolLiteral.FALSE : FBoolLiteral.TRUE;
                case LESS          -> firstFloat <  secondFloat ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case GREATER       -> firstFloat >  secondFloat ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case LESS_EQUAL    -> firstFloat <= secondFloat ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case GREATER_EQUAL -> firstFloat >= secondFloat ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                default -> null;
            };
            if (newLiteral != null)
                return new FLiteralExpression(Position.fromCtx(ctx), newLiteral);
        }

        if (first instanceof FFloat64Literal && second instanceof FFloat64Literal) {
            double firstFloat = ((FFloat64Literal) first).value;
            double secondFloat = ((FFloat64Literal) second).value;
            FLiteral newLiteral = switch (BinaryOperator.getFromParserToken(ctx.getChild(1).getText())) {
                case PLUS    -> new FFloat64Literal(firstFloat + secondFloat);
                case MINUS   -> new FFloat64Literal(firstFloat - secondFloat);
                case TIMES   -> new FFloat64Literal(firstFloat * secondFloat);
                case DIVIDED -> new FFloat64Literal(firstFloat / secondFloat);
                case MODULO  -> new FFloat64Literal(firstFloat % secondFloat);
                case EQUALS, EQUALS_ID         -> firstFloat == secondFloat ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case NOT_EQUALS, NOT_EQUALS_ID -> firstFloat != secondFloat ? FBoolLiteral.FALSE : FBoolLiteral.TRUE;
                case LESS          -> firstFloat <  secondFloat ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case GREATER       -> firstFloat >  secondFloat ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case LESS_EQUAL    -> firstFloat <= secondFloat ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case GREATER_EQUAL -> firstFloat >= secondFloat ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                default -> null;
            };
            if (newLiteral != null)
                return new FLiteralExpression(Position.fromCtx(ctx), newLiteral);
        }

        if (first instanceof FBoolLiteral && second instanceof FBoolLiteral) {
            boolean firstBool = ((FBoolLiteral) first).value;
            boolean secondBool = ((FBoolLiteral) second).value;
            FLiteral newLiteral = switch (BinaryOperator.getFromParserToken(ctx.getChild(1).getText())) {
                case AND, AAND -> firstBool & secondBool ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case OR, AOR   -> firstBool | secondBool ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case XOR       -> firstBool ^ secondBool ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case EQUALS, EQUALS_ID         -> firstBool == secondBool ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                case NOT_EQUALS, NOT_EQUALS_ID -> firstBool != secondBool ? FBoolLiteral.TRUE : FBoolLiteral.FALSE;
                default -> null;
            };
            if (newLiteral != null)
                return new FLiteralExpression(Position.fromCtx(ctx), newLiteral);
        }

        return null;
    }

    @Override
    public FFunctionCall visitCast(FrontierParser.CastContext ctx) {
        FExpression castedExpression = visitExpression(ctx.expression());
        if (!(castedExpression.getType() instanceof FOptional)) { //TODO this should be changed to FOptional.canBeTreatedAsOptional, but I need to decide on the semantics of using ! on optional tuples
            errors.add(new NonOptionalExMark(castedExpression));
            throw new Failed();
        }

        try {
            return functionCall(Position.fromCtx(ctx), castedExpression.getType().getNamespace(), FOptional.EXMARK, mutableSingletonList(castedExpression), emptyMap(), false);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
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
        var arguments = visitArguments(ctx.arguments());
        arguments.a.add(0, array);
        try {
            return functionCall(Position.fromCtx(ctx), array.getType().getNamespace(), Access.ID, arguments.a, arguments.b, lhsResolve);
        } catch (SyntaxError error) {
            errors.add(error);
            throw new Failed();
        }
    }

    @Override
    public Pair<List<FExpression>, Map<FIdentifier, FExpression>> visitArguments(FrontierParser.ArgumentsContext ctx) {
        if (ctx == null)
            return new Pair<>(emptyList(), emptyMap());
        return new Pair<>(visitExpressionList(ctx.expressionList()), visitNamedExpressions(ctx.namedExpressions()));
    }

    @Override
    public List<FExpression> visitExpressionList(FrontierParser.ExpressionListContext ctx) {
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
    public Map<FIdentifier, FExpression> visitNamedExpressions(FrontierParser.NamedExpressionsContext ctx) { //TODO this doesn't follow the fail paradigm
        if (ctx == null)
            return emptyMap();
        Map<FIdentifier, FExpression> res = new HashMap<>();
        for (FrontierParser.NamedExpressionContext c : ctx.namedExpression()) {
            res.put(new FIdentifier(c.IDENTIFIER().getText()), visitExpression(c.expression()));
        }
        return res;
    }

    private FFunctionCall functionCall(Position position, Namespace clazz, FIdentifier identifier, List<FExpression> positionalArgs, Map<FIdentifier, FExpression> keywordArgs, boolean lhsResolve)
            throws FunctionNotFound, AccessForbidden, IncompatibleTypes {
        List<UnboundExpression> unbounds = new ArrayList<>();
        for (FExpression arg : positionalArgs)
            if (arg instanceof UnboundExpression)
                unbounds.add((UnboundExpression) arg);
        for (FExpression arg : keywordArgs.values())
            if (arg instanceof UnboundExpression)
                unbounds.add((UnboundExpression) arg);

        FunctionResolver.Result res = clazz.resolveFunction(identifier, Utils.typesFromExpressionList(positionalArgs), Utils.typesFromExpressionMap(keywordArgs), null, lhsResolve, unbounds);
        checkAccessForbidden(position, res.getFunction());

        List<FExpression> callArguments = res.argMatching.createArgList(positionalArgs, keywordArgs, e -> new Unpack(e).getUnpackedElements());

        List<FExpression> casted = new ArrayList<>();
        for (Pair<FExpression, FParameter> pair : zip(callArguments, res.signature.getParameters())) {
            if (pair.a == null) {
                casted.add(null);
                continue;
            }

            casted.add(pair.a.typeCheck(pair.b.getType()));
        }

        return FFunctionCall.create(position, res.signature, casted);
    }

    @Override
    public FExpression visitExternalFunctionCall(FrontierParser.ExternalFunctionCallContext ctx) {
        boolean lhsResolve = useLhsresolve;
        useLhsresolve = false;
        FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
        Namespace namespace;

        try {
            FExpression object = visitExpression(ctx.expression());
            var arguments = visitArguments(ctx.arguments());
            if (object instanceof FNamespaceExpression) {
                namespace = ((FNamespaceExpression) object).getNamespace();
            } else {
                namespace = object.getType().getNamespace();
                if (arguments.a.isEmpty())
                    arguments.a = singletonList(object);
                else
                    arguments.a.add(0, object);
            }
            return functionCall(Position.fromCtx(ctx), namespace, identifier, arguments.a, arguments.b, lhsResolve);
        } catch (SyntaxError e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FExpression visitLambdaExpr(FrontierParser.LambdaExprContext ctx) {
        //TODO remember that this lambda is here to check whether it was casted
        //lambda parsing is deferred until we know the argument types
        FrontierParser.LambdaContext lambdaCtx = ctx.lambda();
        FIdentifier id = currentNamespace.getFreshLambdaName();
        Location location = new Location(currentNamespace.getLocation().getFile(), Position.fromCtx(lambdaCtx));

        Pair<List<FType>, List<FTypeVariable>> pair = visitLambdaHeader(lambdaCtx.lambdaHeader());
        FTypeVariable returnType = FTypeVariable.create(location, new FIdentifier("ReturnOf" + id.name));

        FFunctionType lambdaType = FFunctionType.from(FTuple.from(pair.a), returnType);
        UnparsedLambda unparsedLambda = new UnparsedLambda(location.getPoint(), id, lambdaType, pair.b, this, lambdaCtx);
        currentFunction().unparsedLambdas.add(lambdaCtx);
        return unparsedLambda;
    }

    @Override
    public Pair<List<FType>, List<FTypeVariable>> visitLambdaHeader(FrontierParser.LambdaHeaderContext ctx) {
        List<FType> paramTypes = new ArrayList<>();
        List<FTypeVariable> unspecifiedParamTypes = new ArrayList<>();
        for (FrontierParser.LambdaParamContext paramCtx : ctx.lambdaParam()) {
            Pair<FType, Optional<FTypeVariable>> pair = visitLambdaParam(paramCtx);
            paramTypes.add(pair.a);
            pair.b.ifPresent(unspecifiedParamTypes::add);
        }
        return new Pair<>(paramTypes, unspecifiedParamTypes);
    }

    @Override
    public Pair<FType, Optional<FTypeVariable>> visitLambdaParam(FrontierParser.LambdaParamContext ctx) {
        TerminalNode idCtx = ctx.IDENTIFIER();
        if (idCtx != null) {
            FrontierParser.TypeTypeContext typeTypeContext = ctx.typeType();
            if (typeTypeContext != null) { //parameter with type
                try {
                    return new Pair<>(ParserContextUtils.getType(typeTypeContext, this::findNamespaceNoThrow), Optional.empty());
                } catch (SyntaxError syntaxError) {
                    errors.add(syntaxError);
                    throw new Failed();
                }
            } else { //parameter without type
                FTypeVariable typeVariable = FTypeVariable.create(new Location(currentNamespace.getLocation().getFile(), Position.fromCtx(ctx)), new FIdentifier("TypeOf" + new FIdentifier(idCtx.getText()).name));
                return new Pair<>(typeVariable, Optional.of(typeVariable));
            }
        } else { //underscore
            FTypeVariable typeVariable = FTypeVariable.create(new Location(currentNamespace.getLocation().getFile(), Position.fromCtx(ctx)), UnnamedIdentifier.get());
            return new Pair<>(typeVariable, Optional.of(typeVariable));
        }
    }

    public FBaseFunction visitLambda(FrontierParser.LambdaContext ctx, FIdentifier identifier, List<FType> newParamTypes, FType newReturnType) {
        currentFunction().unparsedLambdas.remove(ctx);

        Pair<ImmutableList<FParameter>, Map<FIdentifier, FTypeVariable>> lambdaParams = createLambdaParams(ctx.lambdaHeader(), newParamTypes, newReturnType);

        FBaseFunction lambda = new FunctionBuilder(identifier, currentNamespace)
                .setLocation(new Location(currentNamespace.getLocation().getFile(), Position.fromCtx(ctx)))
                .setVisibility(FVisibilityModifier.PRIVATE)
                .setParams(lambdaParams.a)
                .setReturnType(newReturnType)
                .setParameters(lambdaParams.b)
                .build();
        currentNamespace.addFunctionTrusted(lambda);

        return parseLambdaBody(ctx, lambda, newReturnType == null);
    }

    private Pair<ImmutableList<FParameter>, Map<FIdentifier, FTypeVariable>> createLambdaParams(FrontierParser.LambdaHeaderContext ctx, List<FType> types, FType newReturnType) {
        //TODO collision checks? can this override identifiers from outer?
        //TODO would be nice to reuse things from the GlobalIdentifierCollector that does similar thingies
        ImmutableList.Builder<FParameter> builder = ImmutableList.builder();
        Map<FIdentifier, FTypeVariable> typeParameters = new HashMap<>();

        for (Pair<FrontierParser.LambdaParamContext, FType> paramCtxType : zip(ctx.lambdaParam(), types)) {
            TerminalNode idCtx = paramCtxType.a.IDENTIFIER();
            FIdentifier identifier = idCtx != null ? new FIdentifier(idCtx.getText()) : UnnamedIdentifier.get();
            builder.add(FParameter.create(Position.fromCtx(paramCtxType.a), identifier, paramCtxType.b, false));

            if (paramCtxType.b instanceof FTypeVariable) {
                FTypeVariable paramType = (FTypeVariable) paramCtxType.b;
                typeParameters.put(paramType.getIdentifier(), paramType);
            }
        }

        //add return type to typeParameters if necessary
        if (newReturnType instanceof FTypeVariable) {
            FTypeVariable returnType = (FTypeVariable) newReturnType;
            typeParameters.put(returnType.getIdentifier(), returnType);
        }

        return new Pair<>(builder.build(), typeParameters);
    }

    private FBaseFunction parseLambdaBody(FrontierParser.LambdaContext ctx, FBaseFunction lambda, boolean inferReturnType) {
        //suspend all things of the current function (note that lambdas can appear inside lambdas, so this can go arbitrarily deep)
        functionContextStack.addLast(new FunctionContext(lambda));
        try {
            //push params
            currentFunction().declaredVars.push(Utils.asMap(lambda.getSignature().getParameters()));

            if (ctx.expression() != null)
                return parseExpressionLambda(ctx.expression(), lambda, inferReturnType);
            else
                return parseBlockLambda(ctx, lambda, inferReturnType);
        } finally { //restore the outside context
            endCurrentFunction();
        }
    }

    private FBaseFunction parseExpressionLambda(FrontierParser.ExpressionContext ctx, FBaseFunction lambda, boolean inferReturnType) {
        FExpression expression = visitExpression(ctx);

        if (inferReturnType)
            lambda.setInferredReturnTypeForLambda(expression.getType());

        if (lambda.getType() == FTuple.VOID || expression.getType() == FTuple.VOID) {
            lambda.setBody(FBlock.from(new FExpressionStatement(expression.getPosition(), expression)));
        } else {
            try {
                lambda.setBody(FBlock.from(FReturn.create(expression.getPosition(), expression, lambda)));
            } catch (IncompatibleTypes e) {
                errors.add(e);
                throw new Failed();
            }
        }
        return lambda;
    }

    private static final FTypeVariable RETURN_TYPE_PLACEHOLDER = FTypeVariable.create(null, new FIdentifier("!lambdaReturnType"));
    private FBaseFunction parseBlockLambda(FrontierParser.LambdaContext ctx, FBaseFunction lambda, boolean inferReturnType) {
        if (inferReturnType) {
            currentFunction().delayReturnCheck = true;
            currentFunction().returnsToCheck = new ArrayList<>();
        }

        lambda.setBody(visitBlock(ctx.block()));

        if (inferReturnType) {
            try {
                List<ImplicitCastable> constraints = new ArrayList<>(currentFunction().returnsToCheck.size());
                for (FReturn _return : currentFunction().returnsToCheck) {
                    FType type = _return.getExpression().map(Typed::getType).orElse(FTuple.VOID);
                    constraints.add(new ImplicitCastable(_return, type, Variance.Covariant));
                }
                FType returnType = Constraints.resolve(RETURN_TYPE_PLACEHOLDER, constraints);

                // change return type in the function definition
                lambda.setInferredReturnTypeForLambda(returnType);

                // check returns
                for (FReturn _return : currentFunction().returnsToCheck)
                    _return.checkTypes();
            } catch (UnfulfillableConstraints | IncompatibleTypes e) {
                errors.add(e);
                throw new Failed();
            }
        }
        return lambda;
    }

    @Override
    public FExpression visitInternalFunctionCall(FrontierParser.InternalFunctionCallContext ctx) { //TODO this needs far better resolving...
        boolean lhsResolve = useLhsresolve;
        useLhsresolve = false;
        TerminalNode identifierNode = ctx.IDENTIFIER();
        FIdentifier identifier = new FIdentifier(identifierNode.getText());
        Position position = Position.fromCtx(ctx);
        var arguments = visitArguments(ctx.arguments());
        try {
            //first check if we have a dynamic call or a constructor call
            FExpression var = findLocal(Position.fromToken(identifierNode.getSymbol()), identifier, lhsResolve);
            if (var.getType() instanceof FFunctionType) { //dynamic call
                if (!arguments.b.isEmpty())
                    throw new DynamicCallWithKeywordArgs(var, arguments.b);
                return DynamicFunctionCall.create(position, var, arguments.a);
            } else if (var instanceof FNamespaceExpression && arguments.a.isEmpty()) { //constructor call
                return functionCall(position, ((FNamespaceExpression) var).getNamespace(), FConstructor.NEW_ID, arguments.a, arguments.b, lhsResolve);
            }
        } catch (UndeclaredVariable | FunctionNotFound | AccessForbidden ignored) {
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
        } catch (SyntaxError e) {
            //instance method not found, check for static method
            try {
                return functionCall(position, currentNamespace, identifier, arguments.a, arguments.b, lhsResolve);
            } catch (SyntaxError e2) {
                errors.add(e2);
                throw new Failed();
            }
        }
    }

    @Override
    public FFunctionCall visitNewObject(FrontierParser.NewObjectContext ctx) {
        FType type;
        try {
            type = ParserContextUtils.getUserType(ctx.userType(), this::findNamespaceNoThrow);
        } catch (SyntaxError e) {
            errors.add(e);
            visitNamedExpressions(ctx.namedExpressions()); //parse param list to find more errors
            throw new Failed();
        }
        Map<FIdentifier, FExpression> namedArguments = visitNamedExpressions(ctx.namedExpressions());
        try {
            return functionCall(Position.fromCtx(ctx), type.getNamespace(), FConstructor.NEW_ID, emptyList(), namedArguments, false);
        } catch (SyntaxError e) {
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
            return functionCall(Position.fromCtx(ctx), namespace, FConstructor.NEW_ID, mutableSingletonList(expression), emptyMap(), false);
        } catch (SyntaxError e) {
            errors.add(e);
            throw new Failed();
        }
    }


    private static final FTypeVariable ARRAY_LITERAL_TYPE_PLACEHOLDER = FTypeVariable.create(null, new FIdentifier("!arrayLiteralType"));
    @Override
    public FArrayLiteral visitArrayLiteral(ArrayLiteralContext ctx) {
        List<FExpression> elements = visitExpressionList(ctx.expressionList());

        FType elementType;
        TypeOrTupleContext c = ctx.typeOrTuple();
        try {
            if (c != null) {
                elementType = ParserContextUtils.getType(c, this::findNamespaceNoThrow);
            } else {
                elementType = Constraints.resolve(ARRAY_LITERAL_TYPE_PLACEHOLDER, elements.stream().map(e -> new ImplicitCastable(e, e.getType(), Variance.Covariant)).collect(toList()));
            }

            return FArrayLiteral.create(Position.fromCtx(ctx), elementType, elements);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
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

    private FExpression createFunctionAddress(Position position, FFunction function) {
        if (function.getParameters().isEmpty())
            return new FFunctionAddress(position, function);
        return new UninstantiatedGenericFunctionAddress(position, function);
    }

    @Override
    public FExpression visitInternalFunctionAddress(FrontierParser.InternalFunctionAddressContext ctx) {
        try {
            List<FType> params = ctx.typeList() != null ? FTuple.unpackType(ParserContextUtils.tupleFromList(ctx.typeList(), this::findNamespaceNoThrow)) : null;
            FIdentifier identifier;
            if (ctx.IDENTIFIER() != null)
                identifier = new FIdentifier(ctx.IDENTIFIER().getText());
            else
                identifier = Operator.get(ctx.operator().getText(), params.size()).getIdentifier();
            Position pos = Position.fromCtx(ctx);
            FFunction function = getFunction(pos, currentNamespace, identifier, params);
            return createFunctionAddress(pos, function);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    @Override
    public FExpression visitFunctionAddress(FrontierParser.FunctionAddressContext ctx) {
        try {
            Namespace namespace = ParserContextUtils.getNamespace(ctx.typeType(), this::findNamespaceNoThrow);
            List<FType> params = ctx.typeList() != null ? FTuple.unpackType(ParserContextUtils.tupleFromList(ctx.typeList(), this::findNamespaceNoThrow)) : null;
            FIdentifier identifier;
            if (ctx.IDENTIFIER() != null)
                identifier = new FIdentifier(ctx.IDENTIFIER().getText());
            else
                identifier = Operator.get(ctx.operator().getText(), params.size()).getIdentifier();
            Position pos = Position.fromCtx(ctx);
            FFunction function = getFunction(pos, namespace, identifier, params);
            return createFunctionAddress(pos, function);
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    //TODO check why this is different/more complex then functionCall (should functionCall call this?, comment why not)
    private FFunction getFunction(Position position, Namespace namespace, FIdentifier identifier, List<FType> params) throws FunctionNotFound, AccessForbidden {
        if (!(namespace instanceof DefaultNamespace))
            throw new FunctionNotFound(position, identifier, params, emptyMap());
        if (params == null) {
            Collection<Signature> fun = ((DefaultNamespace) namespace).getFunctions(false).get(identifier);  //TODO lhsResolve
            if (fun.size() != 1)
                throw new FunctionNotFound(position, identifier, emptyList(), emptyMap());
            return fun.iterator().next().getFunction();
        } else {
            try {
                FFunction f = namespace.resolveFunction(identifier, params, emptyMap(), null, false, List.of()).getFunction();  //TODO lhsResolve
                checkAccessForbidden(position, f);
                return f;
            } catch (FunctionNotFound fnf) {
                if (currentClass == null)
                    throw fnf;
                params.add(0, currentClass); //static failed, try instance
                FFunction f = namespace.resolveFunction(identifier, params, emptyMap(), null, false, List.of()).getFunction();  //TODO lhsResolve
                checkAccessForbidden(position, f);
                return f;
            }
        }
    }

    @Override
    public Object visitAddressOf(FrontierParser.AddressOfContext ctx) {
        List<FExpression> arguments = visitExpressionList(ctx.expressionList());
        FType baseType = FTuple.from(Utils.typesFromExpressionList(arguments));
        Signature of = getOnlyElement(CArray.getArrayFrom(baseType).getNamespace().getFunctions(false).get(CArray.OF));
        return FFunctionCall.create(Position.fromCtx(ctx), of, arguments);
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

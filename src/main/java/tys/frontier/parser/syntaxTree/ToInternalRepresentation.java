package tys.frontier.parser.syntaxTree;

import tys.frontier.code.*;
import tys.frontier.code.Operator.FUnaryOperator;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.parser.warnings.UnreachableStatements;
import tys.frontier.parser.warnings.Warning;
import tys.frontier.util.MapStack;
import tys.frontier.util.Utils;

import java.util.*;

public class ToInternalRepresentation extends FrontierBaseVisitor {

    private SyntaxTreeData treeData;
    private List<Warning> warnings = new ArrayList<>();
    private List<SyntaxError> errors = new ArrayList<>();

    private FClass currentType;
    private FFunction currentFunction;
    private Deque<FLoopIdentifier> loops = new ArrayDeque<>();
    private MapStack<FIdentifier, FLocalVariable> declaredVars = new MapStack<>();

    private MapStack<FTypeIdentifier, FType> knownClasses = new MapStack<>();


    private ToInternalRepresentation(SyntaxTreeData syntaxTreeData, Module module) {
        this.treeData = syntaxTreeData;
        knownClasses.push();
        knownClasses.putAll(module.getClasses());
        knownClasses.putAll(module.getImportedClasses());
    }

    public static List<Warning> toInternal(SyntaxTreeData syntaxTreeData, Module module) throws SyntaxErrors {
        ToInternalRepresentation visitor = new ToInternalRepresentation(syntaxTreeData, module);
        visitor.generateConstructors();
        visitor.visitFile(syntaxTreeData.root);
        if (!visitor.errors.isEmpty())
            throw SyntaxErrors.create(visitor.errors);
        return visitor.warnings;
    }

    public void generateConstructors() {
        for (FClass fClass : treeData.classes.values()) {
            fClass.generateConstructor();
        }
    }

    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentType = treeData.classes.get(ctx);
        knownClasses.push();
        knownClasses.putAll(currentType.getParameters());
        try {
            return visitChildren(ctx);
        } finally {
            knownClasses.pop();
            currentType = null;
        }
    }

    //fields
    @Override
    public FField visitFieldDeclaration(FrontierParser.FieldDeclarationContext ctx) {
        FField field = treeData.fields.get(ctx);
        if (ctx.expression() != null) {
            declaredVars.push();
            knownClasses.push();
            try {
                if (!field.isStatic()) {
                    FLocalVariable _this = field.getThis();
                    declaredVars.put(_this.getIdentifier(), _this);
                }
                FExpression expression = visitExpression(ctx.expression());
                field.setAssignment(expression); //TODO field assignments need: check for cyclic dependency, register in class/object initializer etc.
                if (!field.isStatic())
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
                declaredVars.pop();
                knownClasses.pop();
            }
        }
        return field;
    }

    private FLocalVariable findLocalVar(FIdentifier identifier) throws UndeclaredVariable {
        FLocalVariable var = declaredVars.get(identifier);
        if (var == null) {
            throw new UndeclaredVariable(identifier);
        }
        return var;
    }

    private void checkAccessForbidden(FTypeMember member) throws AccessForbidden {
        if (currentType != member.getMemberOf() && member.getVisibility() == FVisibilityModifier.PRIVATE) {
            throw new AccessForbidden(member);
        }
    }

    private FField findInstanceField(FType fClass, FIdentifier identifier) throws FieldNotFound, AccessForbidden {
        FField res = fClass.getInstanceFields().get(identifier);
        if (res == null)
            throw new FieldNotFound(identifier);
        checkAccessForbidden(res);
        return res;
    }

    private FField findStaticField(FType type, FIdentifier identifier) throws FieldNotFound, AccessForbidden {
        FField res = type.getStaticFields().get(identifier);
        if (res == null)
            throw new FieldNotFound(identifier);
        checkAccessForbidden(res);
        return res;
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
        currentFunction = f;
        declaredVars.push(Utils.asMap(f.getParams()));
        knownClasses.push();
        try {
            ctx.methodHeader().accept(this);
            f.setBody(visitBlock(ctx.block()));
            return f;
        } finally {
            currentFunction = null;
            declaredVars.pop();
            knownClasses.pop();
        }
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
        return (FStatement) ctx.accept(this);
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
            return FReturn.create(val, currentFunction);
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
        if (declaredVars.contains(identifier)) {
            errors.add(new TwiceDefinedLocalVariable(identifier));
            failed = true;
        }

        //explicit type if present
        FType type = null;
        if (tc != null) {
            try {
                type = ParserContextUtils.getType(tc, knownClasses::get);
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

        if (type == null) { //type inference failed
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
            FTypeVariable typeVar = new FTypeVariable(var);
            knownClasses.put(typeVar.getIdentifier(), typeVar);
        }
        declaredVars.put(identifier, var);
        return FVarDeclaration.create(var, assign);
    }

    @Override
    public FBlock visitBlock(FrontierParser.BlockContext ctx) {
        declaredVars.push();
        knownClasses.push();
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
            declaredVars.pop();
            knownClasses.pop();
        }
    }

    @Override
    public FBlock visitBlockStatement(FrontierParser.BlockStatementContext ctx) {
        return visitBlock(ctx.block());
    }

    @Override
    public FIf visitIfStatement(FrontierParser.IfStatementContext ctx) {
        FExpression cond = null;
        FBlock then = null;
        FBlock elze = null;
        boolean failed = false;
        try {
            cond = visitExpression(ctx.expression());
        } catch (Failed f) {
            failed = true;
        }
        try {
            then = visitBlock(ctx.block(0));
        } catch (Failed f) {
            failed = true;
        }
        try {
            FrontierParser.BlockContext bc = ctx.block(1);
            if (bc != null) {
                elze = visitBlock(ctx.block(1));
            } else if (ctx.ifStatement() != null) {
                elze = FBlock.from(visitIfStatement(ctx.ifStatement()));
            }
        } catch (Failed f) {
            failed = true;
        }

        if (failed)
            throw new Failed();
        try {
            return FIf.create(cond, then, elze);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    @Override
    public FWhile visitWhileStatement(FrontierParser.WhileStatementContext ctx) {
        FLoopIdentifier identifier = new FLoopIdentifier();
        loops.push(identifier);
        declaredVars.push();
        knownClasses.push();
        try {
            FExpression cond;
            try {
                cond = visitExpression(ctx.expression());
            } catch (Failed f) {
                visitBlock(ctx.block()); //still visit the body to find more errors
                throw f;
            }
            FBlock body = visitBlock(ctx.block());
            return FWhile.create(loops.size(), identifier, cond, body);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        } finally {
            loops.pop();
            declaredVars.pop();
            knownClasses.pop();
        }
    }

    @Override
    public FFor visitForStatement(FrontierParser.ForStatementContext ctx) {
        FLoopIdentifier identifier = new FLoopIdentifier();
        loops.push(identifier);
        declaredVars.push();
        knownClasses.push();
        try {
            FVarDeclaration decl = null;
            FExpression cond = null;
            FExpression inc = null;
            FBlock body = null;
            boolean failed = false;

            FrontierParser.LocalVariableDeclarationContext dc = ctx.localVariableDeclaration();
            try {
                decl = dc == null ? null : visitLocalVariableDeclaration(dc);
            } catch (Failed f) {
                failed = true;
            }

            FrontierParser.ExpressionContext ec = ctx.expression();
            try {
                cond = ec == null ? null : visitExpression(ec);
            } catch (Failed f) {
                failed = true;
            }

            FrontierParser.Expression2Context c2 = ctx.expression2();
            try {
                inc = c2 == null ? null : visitExpression(c2.expression());
            } catch (Failed f) {
                failed = true;
            }

            try {
                body = visitBlock(ctx.block());
            } catch (Failed f) {
                failed = true;
            }

            if (failed)
                throw new Failed();

            return FFor.create(loops.size(), identifier, decl, cond, inc, body);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        } finally {
            loops.pop();
            declaredVars.pop();
            knownClasses.pop();
        }
    }

    @Override
    public FForEach visitForeachStatement(FrontierParser.ForeachStatementContext ctx) {
        FLoopIdentifier identifier = new FLoopIdentifier();
        loops.push(identifier);
        declaredVars.push();
        knownClasses.push();
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
            declaredVars.put(it.getIdentifier(), it);
            FBlock body = visitBlock(ctx.block());
            return FForEach.create(loops.size(), identifier, it, container, body);
        } finally {
            loops.pop();
            declaredVars.pop();
            knownClasses.pop();
        }
    }

    @Override
    public FBreak visitBreakStatement(FrontierParser.BreakStatementContext ctx) {
        if (loops.isEmpty()) {
            errors.add(new StatementOutsideLoop());
            throw new Failed();
        }
        return new FBreak(loops.peek());
    }

    @Override
    public FContinue visitContinueStatement(FrontierParser.ContinueStatementContext ctx) {
        if (loops.isEmpty()) {
            errors.add(new StatementOutsideLoop());
            throw new Failed();
        }
        return new FContinue(loops.peek());
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
            return new FLocalVariableExpression(findLocalVar(identifier));
        } catch (UndeclaredVariable ignored) {}
        try {
            return FFieldAccess.createStatic(findStaticField(currentType, identifier));
        } catch (FieldNotFound ignored) {
        } catch (AccessForbidden accessForbidden) {
            errors.add(accessForbidden);
            throw new Failed();
        }
        try {
            return FFieldAccess.createInstance(findInstanceField(currentType, identifier), getThisExpr());
        } catch (FieldNotFound | UndeclaredVariable ignored) {
        } catch (AccessForbidden | IncompatibleTypes syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
        errors.add(new UndeclaredVariable(identifier));
        throw new Failed();
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
                type = ParserContextUtils.getType(ctx.typeType(), knownClasses::get);
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
        FExpression object = visitExpression(ctx.expression());

        try {
            return FFieldAccess.createInstance(findInstanceField(object.getType(), identifier), object);
        } catch (FieldNotFound | AccessForbidden | IncompatibleTypes e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFieldAccess visitStaticFieldAccess(FrontierParser.StaticFieldAccessContext ctx) {
        FIdentifier identifier = ParserContextUtils.getVarIdentifier(ctx.identifier());
        try {
            FType fType = ParserContextUtils.getType(ctx.typeType(), knownClasses::get);
            return FFieldAccess.createStatic(findStaticField(fType, identifier));
        } catch (SyntaxError e) {
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
        FFunction f = clazz.resolveFunction(identifier, params, TypeInstantiation.EMPTY).a;
        checkAccessForbidden(f);
        return FFunctionCall.create(f, params);
    }

    @Override
    public FFunctionCall visitExternalFunctionCall(FrontierParser.ExternalFunctionCallContext ctx) {
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.LCIdentifier().getText());
        List<FExpression> params = new ArrayList<>();
        FType namespace;

        try {
            FExpression object = visitExpression(ctx.expression());
            if (object.getType() == FTypeType.INSTANCE) {
                if (object instanceof FClassExpression)
                    namespace = ((FClassExpression) object).getfClass();
                else
                    return Utils.NYI("a call on an expression of Type FTypeType that is not an FClassExpression");
            } else {
                namespace = object.getType();
                params.add(object);
            }
        } catch (Failed f) {
            visitExpressionList(ctx.expressionList());
            throw f;
        }
        params.addAll(visitExpressionList(ctx.expressionList()));
        try {
            return functionCall(namespace, identifier, params);
        } catch (FunctionNotFound | AccessForbidden | IncompatibleTypes e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitInternalFunctionCall(FrontierParser.InternalFunctionCallContext ctx) { //TODO this needs far better resolving...
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.LCIdentifier().getText());
        List<FExpression> params = visitExpressionList(ctx.expressionList());
        try {
            List<FExpression> params2 = new ArrayList<>(params.size() + 1);
            params2.add(getThisExpr());
            params2.addAll(params);
            return functionCall(currentType, identifier, params2);
        } catch (FunctionNotFound | UndeclaredVariable | AccessForbidden | IncompatibleTypes e) {
            //instance method not found, check for static method
            try {
                return functionCall(currentType, identifier, params);
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
            type = ParserContextUtils.getType(ctx.typeType(), knownClasses::get);
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
            baseType = ParserContextUtils.getType(ctx.typeType(), knownClasses::get);
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
            FType fType = ParserContextUtils.getType(ctx.typeType(), knownClasses::get);
            return new FClassExpression(fType);
        } catch (SyntaxError e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionAddress visitInternalFunctionAddress(FrontierParser.InternalFunctionAddressContext ctx) {
        try {
            List<FType> params = ctx.typeList() != null ? ParserContextUtils.typesFromList(ctx.typeList().typeType(), knownClasses::get) : null;
            return new FFunctionAddress(getFunction(currentType, new FFunctionIdentifier(ctx.LCIdentifier().getText()), params));
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    @Override
    public FFunctionAddress visitFunctionAddress(FrontierParser.FunctionAddressContext ctx) {
        try {
            FType fClass = ParserContextUtils.getType(ctx.typeType(), knownClasses::get);
            List<FType> params = ctx.typeList() != null ? ParserContextUtils.typesFromList(ctx.typeList().typeType(), knownClasses::get) : null;
            return new FFunctionAddress(getFunction(fClass, new FFunctionIdentifier(ctx.LCIdentifier().getText()), params));
        } catch (SyntaxError syntaxError) {
            errors.add(syntaxError);
            throw new Failed();
        }
    }

    private FFunction getFunction(FType fClass, FFunctionIdentifier identifier, List<FType> params) throws FunctionNotFound {
        //TODO use params to resolve better
        Collection<FFunction> fun = fClass.getFunctions().get(identifier);
        if (fun.size() != 1)
            throw new FunctionNotFound(identifier, params);
        return fun.iterator().next();
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

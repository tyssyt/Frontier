package tys.frontier.parser.syntaxTree;

import tys.frontier.code.*;
import tys.frontier.code.Operator.FUnaryOperator;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
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
    private Stack<FLoopIdentifier> loops = new Stack<>();
    private MapStack<FVariableIdentifier, FLocalVariable> declaredVars = new MapStack<>();

    private Map<FTypeIdentifier, FClass> knownClasses;


    private ToInternalRepresentation(SyntaxTreeData syntaxTreeData, Module module) {
        this.treeData = syntaxTreeData;
        knownClasses = new HashMap<>(module.getClasses());
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
        try {
            return visitChildren(ctx);
        } finally {
            currentType = null;
        }
    }

    //fields
    @Override
    public FField visitFieldDeclaration(FrontierParser.FieldDeclarationContext ctx) {
        FField field = treeData.fields.get(ctx);
        if (ctx.expression() != null) {
            declaredVars.push();
            try {
                FFieldAccess access;
                if (field.isStatic()) {
                    access = FFieldAccess.createStatic(field);
                } else {
                    FLocalVariable thiz = currentType.getThis();
                    declaredVars.put(thiz.getIdentifier(), thiz);
                    access = FFieldAccess.createInstanceTrusted(field, new FLocalVariableExpression(thiz));
                }
                FExpression expression = visitExpression(ctx.expression());
                FVarAssignment assignment = FVarAssignment.create(access, FVarAssignment.Operator.ASSIGN, expression);
                field.setAssignment(assignment); //TODO field assignments need: check for cyclic dependency, register in class/object initializer etc.
            } catch (Failed f) {
                //do not allow Failed to propagate any further
            } catch (IncompatibleTypes incompatibleTypes) {
                errors.add(incompatibleTypes);
            } finally {
                declaredVars.pop();
            }
        }
        return field;
    }

    private FLocalVariable findLocalVar(FVariableIdentifier identifier) throws UndeclaredVariable {
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

    private FField findInstanceField(FClass fClass, FVariableIdentifier identifier) throws FieldNotFound, AccessForbidden {
        FField res = fClass.getInstanceFields().get(identifier);
        if (res == null)
            throw new FieldNotFound(identifier);
        checkAccessForbidden(res);
        return res;
    }

    private FField findStaticField(FClass type, FVariableIdentifier identifier) throws FieldNotFound, AccessForbidden {
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
        try {
            ctx.methodHeader().accept(this);

            //parse function body
            if (!f.isStatic()) {
                FLocalVariable thiz = currentType.getThis();
                declaredVars.put(thiz.getIdentifier(), thiz);
            }

            f.setBody(visitBlock(ctx.block()));
            return f;
        } finally {
            currentFunction = null;
            declaredVars.pop();
        }
    }

    @Override
    public Object visitFormalParameter(FrontierParser.FormalParameterContext ctx) {
        FrontierParser.ExpressionContext c = ctx.expression();
        if (c != null) {
            treeData.parameters.get(ctx).setDefaultValue(visitExpression(c));
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
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        if (declaredVars.contains(identifier)) {
            errors.add(new TwiceDefinedLocalVariable(identifier));
            failed = true;
        }

        //explicit type if present
        FClass type = null;
        if (tc != null) {
            try {
                type = ParserContextUtils.getType(tc, knownClasses);
            } catch (TypeNotFound e) {
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

        declaredVars.put(identifier, var);
        return FVarDeclaration.create(var, assign);
    }

    @Override
    public FBlock visitBlock(FrontierParser.BlockContext ctx) {
        declaredVars.push();
        try {
            List<FStatement> statements = statementsFromList(ctx.statement());
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
        }
    }

    @Override
    public FFor visitForStatement(FrontierParser.ForStatementContext ctx) {
        FLoopIdentifier identifier = new FLoopIdentifier();
        loops.push(identifier);
        declaredVars.push();
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
        }
    }

    @Override
    public FForEach visitForeachStatement(FrontierParser.ForeachStatementContext ctx) {
        FLoopIdentifier identifier = new FLoopIdentifier();
        loops.push(identifier);
        declaredVars.push();
        try {
            FExpression container = visitExpression(ctx.expression());
            FVariableIdentifier id = new FVariableIdentifier(ctx.Identifier().getText());
            FClass varType;
            if (container.getType() instanceof FArray) {
                varType = ((FArray) container.getType()).getOneDimensionLess();
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
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
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
            return instanceFunctionCall(expression, identifier, Collections.emptyList());
        } catch (FunctionNotFound | AccessForbidden e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public Object visitPostUnaryOp(FrontierParser.PostUnaryOpContext ctx) {
        FExpression expression = visitExpression(ctx.expression());
        FFunctionIdentifier identifier = new FFunctionIdentifier('_' + ctx.getChild(1).getText());

        if(expression.getType() instanceof FPredefinedClass &&
                (identifier.equals(FUnaryOperator.Post.INC.identifier) || identifier.equals(FUnaryOperator.Post.DEC.identifier))) {
            //special case for inc and dec on predefined types, they are both write and read
            if ((expression instanceof FVariableExpression)) {
                ((FVariableExpression) expression).setAccessType(FVariableExpression.AccessType.LOAD_AND_STORE);
            } else {
                errors.add(new NonAssignableExpression(expression));
                throw new Failed();
            }
        }

        try {
            return instanceFunctionCall(expression, identifier, Collections.emptyList());
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
            return staticFunctionCall(first.getType(), identifier, Arrays.asList(first, second));
        } catch (FunctionNotFound | AccessForbidden e1) {
            try {
                return staticFunctionCall(second.getType(), identifier, Arrays.asList(first, second));
            } catch (FunctionNotFound | AccessForbidden e2) {
                errors.add(e1);
                errors.add(e2);
                throw new Failed();
            }
        }
    }

    @Override
    public FExplicitCast visitCast(FrontierParser.CastContext ctx) {
        FClass type;
        try {
            type = ParserContextUtils.getType(ctx.typeType(), knownClasses);
        } catch (TypeNotFound typeNotFound) {
            errors.add(typeNotFound);
            visitExpression(ctx.expression());
            throw new Failed();
        }
        FExpression castedExpression = visitExpression(ctx.expression());
        try {
            return new FExplicitCast(type, castedExpression);
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
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
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
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        try {
            FClass fClass = ParserContextUtils.getType(ctx.typeType(), knownClasses);
            return FFieldAccess.createStatic(findStaticField(fClass, identifier));
        } catch (TypeNotFound | FieldNotFound | AccessForbidden e) {
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
    private List<FClass> typesFromExpressionList (List<FExpression> exps) {
        List<FClass> res = new ArrayList<>(exps.size());
        for (FExpression exp : exps)
            res.add(exp.getType());
        return res;
    }

    private FFunctionCall instanceFunctionCall (FExpression object, FFunctionIdentifier identifier, List<FExpression> params)
            throws FunctionNotFound, AccessForbidden {
        List<FClass> paramTypes = typesFromExpressionList(params);
        FFunction f = object.getType().resolveInstanceFunction(identifier, paramTypes).a;
        checkAccessForbidden(f);
        try {
            return FFunctionCall.createInstance(object, f, params);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    private FFunctionCall staticFunctionCall (FClass clazz, FFunctionIdentifier identifier, List<FExpression> params)
            throws FunctionNotFound, AccessForbidden {
        List<FClass> paramTypes = typesFromExpressionList(params);
        FFunction f = clazz.resolveStaticFunction(identifier, paramTypes).a;
        checkAccessForbidden(f);
        try {
            return FFunctionCall.createStatic(f, params);
        } catch (IncompatibleTypes incompatibleTypes) {
            errors.add(incompatibleTypes);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitExternalFunctionCall(FrontierParser.ExternalFunctionCallContext ctx) {
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
        FExpression object;
        try {
            object = visitExpression(ctx.expression());
        } catch (Failed f) {
            visitExpressionList(ctx.expressionList());
            throw f;
        }
        List<FExpression> params = visitExpressionList(ctx.expressionList());
        try {
            return instanceFunctionCall(object, identifier, params);
        } catch (FunctionNotFound | AccessForbidden e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitInternalFunctionCall(FrontierParser.InternalFunctionCallContext ctx) { //TODO this needs far better resolving...
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
        List<FExpression> params = visitExpressionList(ctx.expressionList());
        try {
            return instanceFunctionCall(getThisExpr(), identifier, params);
        } catch (FunctionNotFound | UndeclaredVariable | AccessForbidden e) {
            //instance method not found, check for static method
            try {
                return staticFunctionCall(currentType, identifier, params);
            } catch (FunctionNotFound |AccessForbidden functionNotFound) {
                errors.add(functionNotFound);
                throw new Failed();
            }
        }
    }

    @Override
    public FFunctionCall visitStaticFunctionCall(FrontierParser.StaticFunctionCallContext ctx) {
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
        List<FExpression> params = visitExpressionList(ctx.expressionList());
        try {
            FClass clazz = ParserContextUtils.getType(ctx.typeType(), knownClasses);
            return staticFunctionCall(clazz, identifier, params);
        } catch (TypeNotFound | FunctionNotFound | AccessForbidden e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitNewObject(FrontierParser.NewObjectContext ctx) {
        FClass clazz;
        try {
            clazz = ParserContextUtils.getBasicType(ctx.basicType(), knownClasses);
        } catch (TypeNotFound e) {
            errors.add(e);
            visitExpressionList(ctx.expressionList()); //parse param list to find more errors
            throw new Failed();
        }
        List<FExpression> params = visitExpressionList(ctx.expressionList());
        try {
            return staticFunctionCall(clazz, FConstructor.IDENTIFIER, params);
        } catch (FunctionNotFound | AccessForbidden e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitNewArray(FrontierParser.NewArrayContext ctx) {
        List<FrontierParser.ExpressionContext> cs = ctx.expression();
        int initialisedDepth = cs.size();
        int uninitialisedDepth = ctx.Array().size();

        List<FExpression> params = new ArrayList<>(initialisedDepth);
        boolean failed = false;
        for (FrontierParser.ExpressionContext c : cs) {
            try {
                params.add(visitExpression(c));
            } catch (Failed f) {
                failed = true;
            }
        }

        FClass baseClass;
        try {
            baseClass = ParserContextUtils.getBasicType(ctx.basicType(), knownClasses);
        } catch (TypeNotFound e) {
            errors.add(e);
            throw new Failed();
        }
        if (failed)
            throw new Failed();

        FArray array = FArray.getArrayFrom(baseClass, initialisedDepth+uninitialisedDepth);
        try {
            return staticFunctionCall(array, FConstructor.IDENTIFIER, params);
        } catch (FunctionNotFound | AccessForbidden e) {
            errors.add(e);
            throw new Failed();
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

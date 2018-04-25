package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.*;
import tys.frontier.code.Operator.FOperator;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.MapStack;
import tys.frontier.util.Utils;

import java.util.*;

public class ToInternalRepresentation extends FrontierBaseVisitor {

    private FFile file;
    private SyntaxTreeData treeData;
    private List<SyntaxError> errors = new ArrayList<>();
    private List<NeedsTypeCheck> typeChecks = new ArrayList<>();

    private FClass currentClass;
    private FFunction currentFunction;
    private Stack<FLoopIdentifier> loops = new Stack<>();
    private MapStack<FVariableIdentifier, FLocalVariable> declaredVars = new MapStack<>();

    private Map<FClassIdentifier, FClass> knownClasses;


    private ToInternalRepresentation(FFile file, SyntaxTreeData syntaxTreeData, Map<FClassIdentifier, FClass> importedClasses) {
        this.file = file;
        this.treeData = syntaxTreeData;
        knownClasses = new HashMap<>(importedClasses);
        knownClasses.putAll(file.getClasses());
    }

    public static List<NeedsTypeCheck> toInternal(SyntaxTreeData syntaxTreeData, FFile file, Map<FClassIdentifier, FClass> importedClasses) throws SyntaxErrors {
        ToInternalRepresentation visitor = new ToInternalRepresentation(file, syntaxTreeData, importedClasses);
        visitor.visitFile(syntaxTreeData.root);
        if (!visitor.errors.isEmpty())
            throw SyntaxErrors.create(visitor.errors);
        return visitor.typeChecks;
    }



    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentClass = treeData.classes.get(ctx);
        try {
            return visitChildren(ctx);
        } finally {
            currentClass = null;
        }
    }

    //fields
    @Override
    public FField visitFieldDeclaration(FrontierParser.FieldDeclarationContext ctx) {
        FrontierParser.VariableDeclaratorContext c = ctx.variableDeclarator();
        FField field = treeData.fields.get(ctx);
        if (c.expression() != null) {
            declaredVars.push();
            if (!field.isStatic()) {
                FLocalVariable thiz = currentClass.getThis();
                declaredVars.put(thiz.getIdentifier(), thiz);
            }
            try {
                FVarDeclaration decl = visitVariableDeclarator(c);
                decl.getAssignment().ifPresent(field::setAssignment); //TODO field assignments need: check for cyclic dependency, register in class/object initializer etc.
            } catch (Failed f) {
                //do not allow Failed to propagate any further
            } finally {
                declaredVars.pop();
            }
        }
        return field;
    }

    private FLocalVariable findLocalVar(FVariableIdentifier identifier) throws UndeclaredVariable {
        FLocalVariable var = declaredVars.get(identifier); //first check local vars
        if (var == null) {
            throw new UndeclaredVariable(identifier);
        }
        return var;
    }

    private FVariable findVar(FVariableIdentifier identifier) throws UndeclaredVariable {
        FVariable var = declaredVars.get(identifier); //first check local vars
        if (var == null) {
            var = currentClass.getField(identifier);  //then try fields
        }
        if (var == null) {
            throw new UndeclaredVariable(identifier);
        }
        return var;
    }

    @Override
    public FVarDeclaration visitVariableDeclarator(FrontierParser.VariableDeclaratorContext ctx) { //FIXME this is all bullshit and does not work with fields
        FrontierParser.ExpressionContext c = ctx.expression();
        FLocalVariable var;
        try {
            var = ParserContextUtils.getVariable(ctx.typedIdentifier(), knownClasses);
        } catch (ClassNotFound e) {
            errors.add(e);
            if (c != null)
                visitExpression(c); //still visit the expression to find more errors
            throw new Failed();
        }

        FVarAssignment assign = null;
        if (c != null) {
            FExpression val = visitExpression(c);
            assign = new FVarAssignment(new FLocalVariableExpression(var), FVarAssignment.Operator.ASSIGN, val);
            typeChecks.add(assign);
        }
        return new FVarDeclaration(var, assign);
    }

    //methods enter & exitArrayAccess

    private ImmutableList<FStatement> statementsFromList (List<FrontierParser.StatementContext> contexts) {
        ImmutableList.Builder<FStatement> builder = new ImmutableList.Builder<>();
        for (FrontierParser.StatementContext c : contexts) {
            try {
                FStatement statement = visitStatement(c);
                if (statement instanceof FEmptyStatement)
                    continue;
                builder.add(statement);
            } catch (Failed f) {
                //this is fine, failed statements are not added to the list, they will have raised errors
            }
        }
        return builder.build();
    }

    @Override
    public FFunction visitMethodDeclaration(FrontierParser.MethodDeclarationContext ctx) {
        FFunction f = treeData.functions.get(ctx);
        currentFunction = f;
        declaredVars.push(Utils.asMap(f.getParams()));
        try {
            if (!f.isStatic()) {
                FLocalVariable thiz = currentClass.getThis();
                declaredVars.put(thiz.getIdentifier(), thiz);
            }
            f.setBody(statementsFromList(ctx.statement()));
            return f;
        } finally {
            currentFunction = null;
            declaredVars.pop();
        }
    }

    @Override
    public FConstructor visitConstructorDeclaration(FrontierParser.ConstructorDeclarationContext ctx) {
        FConstructor f = treeData.constructors.get(ctx);
        currentFunction = f;
        declaredVars.push(Utils.asMap(f.getParams()));
        try {
            FLocalVariable thiz = currentClass.getThis();
            declaredVars.put(thiz.getIdentifier(), thiz);
            f.setBody(statementsFromList(ctx.statement()));
            return f;
        } finally {
            currentFunction = null;
            declaredVars.pop();
        }
    }

    //statements
    public FStatement visitStatement(FrontierParser.StatementContext ctx) throws Failed {
        return (FStatement) ctx.accept(this);
    }

    @Override
    public FEmptyStatement visitEmptyStatement(FrontierParser.EmptyStatementContext ctx) {
        return new FEmptyStatement();
    }

    @Override
    public FExpressionStatement visitExpressionStatement(FrontierParser.ExpressionStatementContext ctx) {
        return new FExpressionStatement(visitExpression(ctx.expression()));
    }

    @Override
    public FReturn visitReturnStatement(FrontierParser.ReturnStatementContext ctx) {
        FrontierParser.ExpressionContext c = ctx.expression();
        FExpression val = c == null ? null : visitExpression(c);
        FReturn res = new FReturn(val, currentFunction);
        typeChecks.add(res);
        return res;
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
        FVarAssignment res = new FVarAssignment(var, op, value);
        typeChecks.add(res);
        return res;
    }

    @Override
    public FVarDeclaration visitLocalVariableDeclarationStatement(FrontierParser.LocalVariableDeclarationStatementContext ctx) {
        FVarDeclaration res = visitVariableDeclarator(ctx.variableDeclarator());
        FVariableIdentifier identifier = res.getVar().getIdentifier();
        if (declaredVars.contains(identifier)) {
            errors.add(new TwiceDefinedLocalVariable(identifier));
            throw new Failed();
        }
        declaredVars.put(res.getVar().getIdentifier(), res.getVar());
        return res;
    }

    @Override
    public FStatement visitBlockStatement(FrontierParser.BlockStatementContext ctx) {
        declaredVars.push();
        try {
            ImmutableList<FStatement> statements = statementsFromList(ctx.statement());
            if (statements.size() == 0)
                return new FEmptyStatement();
            if (statements.size() == 1)
                return statements.get(0);
            return new FBlock(statements);
        } finally {
            declaredVars.pop();
        }
    }

    @Override
    public FIf visitIfStatement(FrontierParser.IfStatementContext ctx) {
        FExpression cond = null;
        FStatement then = null;
        FStatement elze = null;
        boolean failed = false;
        try {
            cond = visitExpression(ctx.expression());
        } catch (Failed f) {
            failed = true;
        }
        try {
            then = visitStatement(ctx.statement(0));
        } catch (Failed f) {
            failed = true;
        }
        FrontierParser.StatementContext sc = ctx.statement(1);
        try {
            elze = sc == null ? null : visitStatement(sc);
        } catch (Failed f) {
            failed = true;
        }
        if (failed)
            throw new Failed();
        FIf res = new FIf(cond, then, elze);
        typeChecks.add(res);
        return res;
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
                visitStatement(ctx.statement()); //still visit the body to find more errors
                throw f;
            }
            FStatement body = visitStatement(ctx.statement());
            FWhile res = new FWhile(identifier, cond, body);
            typeChecks.add(res);
            return res;
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
            FStatement body = null;
            boolean failed = false;

            FrontierParser.VariableDeclaratorContext dc = ctx.variableDeclarator();
            try {
                decl = dc == null ? null : visitVariableDeclarator(dc);
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

            FrontierParser.StatementContext sc = ctx.statement();
            try {
                body = sc == null ? null : visitStatement(sc);
            } catch (Failed f) {
                failed = true;
            }

            if (failed)
                throw new Failed();

            FFor res = new FFor(identifier, decl, cond, inc, body);
            typeChecks.add(res);
            return res;
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
            FLocalVariable it = null;
            FExpression container = null;
            FStatement body = null;
            boolean failed = false;

            try {
                it = ParserContextUtils.getVariable(ctx.typedIdentifier(), knownClasses);
            } catch (ClassNotFound e) {
                errors.add(e);
                failed = true;
            }

            try {
                container = visitExpression(ctx.expression());
            } catch (Failed f) {
                failed = true;
            }

            if (it != null) {
                declaredVars.put(it.getIdentifier(), it);
            }
            try {
                body = visitStatement(ctx.statement());
            } catch (Failed f) {
                failed = true;
            }

            if (failed)
                throw new Failed();

            FForEach res = new FForEach(identifier, it, container, body);
            typeChecks.add(res);
            return res;
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
            FVariable var = findVar(identifier);
            if (var instanceof FLocalVariable)
                return new FLocalVariableExpression((FLocalVariable) var);
            else if (var instanceof FField) {
                FField field = ((FField) var);
                if (field.isStatic())
                    return new FFieldAccess(field);
                else
                    return new FFieldAccess(field, getThisExpr());
            } else {
                throw new RuntimeException();
            }
        } catch (UndeclaredVariable e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FBracketsExpression visitBracketsExpr(FrontierParser.BracketsExprContext ctx) {
        return new FBracketsExpression(visitExpression(ctx.expression()));
    }

    @Override
    public FFunctionCall visitPreUnaryOp(FrontierParser.PreUnaryOpContext ctx) {
        FExpression expression = visitExpression(ctx.expression());
        FFunctionIdentifier identifier = new FFunctionIdentifier('_' + ctx.getChild(0).getText());

        try {
            return staticFunctionCall(expression.getType(), identifier, ImmutableList.of(expression));
        } catch (FunctionNotFound e) {
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
            return staticFunctionCall(first.getType(), identifier, ImmutableList.of(first, second));
        } catch (FunctionNotFound e1) {
            try {
                return staticFunctionCall(second.getType(), identifier, ImmutableList.of(first, second));
            } catch (FunctionNotFound e2) {
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
        } catch (ClassNotFound classNotFound) {
            errors.add(classNotFound);
            visitExpression(ctx.expression());
            throw new Failed();
        }
        FExpression castedExpression = visitExpression(ctx.expression());
        FExplicitCast res = new FExplicitCast(type, castedExpression);
        typeChecks.add(res);
        return res;
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
            return new FArrayAccess(array, index);
        } catch (IncompatibleTypes e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFieldAccess visitFieldAccess(FrontierParser.FieldAccessContext ctx) {
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        FExpression object = visitExpression(ctx.expression());
        FField f = object.getType().getField(identifier);
        if (f == null) {
            errors.add(new FieldNotFound(identifier));
            throw new Failed();
        }
        return new FFieldAccess(f, object);
    }

    @Override
    public FFieldAccess visitStaticFieldAccess(FrontierParser.StaticFieldAccessContext ctx) {
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        try {
            FField f = ParserContextUtils.getType(ctx.typeType(), knownClasses).getField(identifier);
            if (f == null)
                throw new FieldNotFound(identifier);
            return new FFieldAccess(f);
        } catch (ClassNotFound | FieldNotFound e) {
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
    private FFunctionCall functionCall (FExpression object, FFunctionIdentifier identifier, List<FExpression> params)
            throws FunctionNotFound {
        List<FClass> paramTypes = typesFromExpressionList(params);
        FFunction.Signature signature = new FFunction.Signature(identifier, paramTypes);
        FFunction f = object.getType().resolveFunction(signature).a;
        return new FFunctionCall(object, f, params);
    }

    private FFunctionCall staticFunctionCall (FClass clazz, FFunctionIdentifier identifier, List<FExpression> params)
            throws FunctionNotFound {
        List<FClass> paramTypes = typesFromExpressionList(params);
        FFunction.Signature signature = new FFunction.Signature(identifier, paramTypes);
        FFunction f = clazz.resolveFunction(signature).a;
        return new FFunctionCall(f, params);
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
            return functionCall(object, identifier, params);
        } catch (FunctionNotFound e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitInternalFunctionCall(FrontierParser.InternalFunctionCallContext ctx) {
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
        List<FExpression> params = visitExpressionList(ctx.expressionList());
        try {
            return functionCall(getThisExpr(), identifier, params);
        } catch (FunctionNotFound | UndeclaredVariable e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitStaticFunctionCall(FrontierParser.StaticFunctionCallContext ctx) {
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
        List<FExpression> params = visitExpressionList(ctx.expressionList());
        try {
            FClass clazz = ParserContextUtils.getType(ctx.typeType(), knownClasses);
            return staticFunctionCall(clazz, identifier, params);
        } catch (ClassNotFound | FunctionNotFound e) {
            errors.add(e);
            throw new Failed();
        }
    }

    @Override
    public FFunctionCall visitNewObject(FrontierParser.NewObjectContext ctx) {
        FClass clazz;
        try {
            clazz = ParserContextUtils.getBasicType(ctx.basicType(), knownClasses);
        } catch (ClassNotFound e) {
            errors.add(e);
            visitExpressionList(ctx.expressionList()); //parse param list to find more errors
            throw new Failed();
        }
        List<FExpression> params = visitExpressionList(ctx.expressionList());
        try {
            return staticFunctionCall(clazz, FOperator.CONSTRUCTOR, params);
        } catch (FunctionNotFound e) {
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
        } catch (ClassNotFound e) {
            errors.add(e);
            throw new Failed();
        }
        if (failed)
            throw new Failed();

        FArray array = FArray.getArrayFrom(baseClass, initialisedDepth+uninitialisedDepth);
        try {
            return staticFunctionCall(array, FOperator.CONSTRUCTOR, params);
        } catch (FunctionNotFound e) {
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

package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import tys.frontier.code.*;
import tys.frontier.code.Operator.Operators;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.parser.FrontierBaseListener;
import tys.frontier.parser.FrontierParser;
import tys.frontier.parser.syntaxTree.syntaxErrors.*;
import tys.frontier.util.MapStack;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;

public class ToInternalRepresentation extends FrontierBaseListener {

    private FFile file;
    private SyntaxTreeData treeData;
    private List<SyntaxError> errors = new ArrayList<>();
    private List<NeedsTypeCheck> typeChecks = new ArrayList<>();

    private FClass currentClass;
    private FFunction currentFunction;
    private Stack<FLoopIdentifier> loops = new Stack<>();
    private MapStack<FVariableIdentifier, FLocalVariable> declaredVars = new MapStack<>();


    private ToInternalRepresentation(FFile file, SyntaxTreeData syntaxTreeData) {
        this.file = file;
        this.treeData = syntaxTreeData;
    }

    public static List<NeedsTypeCheck> toInternal(SyntaxTreeData syntaxTreeData, FFile file) throws SyntaxErrors {
        ToInternalRepresentation listener = new ToInternalRepresentation(file, syntaxTreeData);
        ParseTreeWalker.DEFAULT.walk(listener, syntaxTreeData.root);
        if (!listener.errors.isEmpty())
            throw new SyntaxErrors(listener.errors);
        return listener.typeChecks;
    }

    @Override
    public void enterClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentClass = treeData.classes.get(ctx);
    }

    //fields
    @Override
    public void exitFieldDeclaration(FrontierParser.FieldDeclarationContext ctx) {
        FrontierParser.VariableDeclaratorContext c = ctx.variableDeclarator();
        if (c.expression() != null) {
            try {
                FVarDeclaration decl = fromVariableDeclarator(c);
                FField field = treeData.fields.get(ctx);
                decl.getAssignment().ifPresent(field::setAssignment);
            } catch (ClassNotFound e) {
                throw new RuntimeException(e); //this should not happen
            }
        }
    }

    private FVariable findVar(FVariableIdentifier identifier) throws UndeclaredVariable {
        FVariable var = declaredVars.get(identifier);
        if (var == null) {
            var = currentClass.getField(identifier);
        }
        if (var == null) {
            throw new UndeclaredVariable(identifier, currentFunction);
        }
        return var;
    }

    private FVarDeclaration fromVariableDeclarator(
            FrontierParser.VariableDeclaratorContext ctx) throws ClassNotFound {
        FLocalVariable var = ParserContextUtils.getVariable(ctx.typedIdentifier(), file.getClasses());

        FVarAssignment assign = null;
        FrontierParser.ExpressionContext c = ctx.expression();
        if (c != null) {
            FExpression val = treeData.expressionMap.get(c);
            assign = new FVarAssignment(var, FVarAssignment.Operator.ASSIGN, val);
            typeChecks.add(assign);
        }
        return new FVarDeclaration(var, assign);
    }

    //methods enter & exitArrayAccess
    @Override
    public void enterMethodDeclaration(FrontierParser.MethodDeclarationContext ctx) {
        FFunction f = treeData.functions.get(ctx);
        currentFunction = f;
        declaredVars.push(Utils.asMap(f.getParams()));
        if (!f.isStatic()) {
            FLocalVariable thiz = currentClass.getThis();
            declaredVars.put(thiz.getIdentifier(), thiz);
        }
    }

    private ImmutableList<FStatement> statementsFromList (List<FrontierParser.StatementContext> contexts) {
        ImmutableList.Builder<FStatement> builder = new ImmutableList.Builder<>();
        for (FrontierParser.StatementContext c : contexts) {
            builder.add(treeData.statementMap.get(c));
        }
        return builder.build();
    }

    @Override
    public void exitMethodDeclaration(FrontierParser.MethodDeclarationContext ctx) {
        currentFunction.setBody(statementsFromList(ctx.statement()));
        currentFunction = null;
        declaredVars.pop();
    }

    @Override
    public void enterConstructorDeclaration(FrontierParser.ConstructorDeclarationContext ctx) {
        FConstructor c = treeData.constructors.get(ctx);
        currentFunction = c;
        declaredVars.push(Utils.asMap(c.getParams()));
        FLocalVariable thiz = currentClass.getThis();
        declaredVars.put(thiz.getIdentifier(), thiz);
    }

    @Override
    public void exitConstructorDeclaration(FrontierParser.ConstructorDeclarationContext ctx) {
        currentFunction.setBody(statementsFromList(ctx.statement()));
        currentFunction = null;
        declaredVars.pop();
    }


    //statements

    @Override
    public void exitEmptyStatement(FrontierParser.EmptyStatementContext ctx) {
        treeData.statementMap.put(ctx, new FEmptyStatement());
    }

    @Override
    public void exitExpressionStatement(FrontierParser.ExpressionStatementContext ctx) {
        FExpressionStatement res = new FExpressionStatement(treeData.expressionMap.get(ctx.expression()));
        treeData.statementMap.put(ctx, res);
    }

    @Override
    public void exitReturnStatement(FrontierParser.ReturnStatementContext ctx) {
        FReturn res = new FReturn(treeData.expressionMap.get(ctx.expression()), currentFunction);
        treeData.statementMap.put(ctx, res);
        typeChecks.add(res);
    }

    @Override
    public void exitAssignment(FrontierParser.AssignmentContext ctx) {
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        try {
            FVariable var = findVar(identifier);
            FVarAssignment.Operator op = FVarAssignment.Operator.fromString(ctx.getChild(1).getText());
            FExpression value = treeData.expressionMap.get(ctx.expression());
            FVarAssignment res = new FVarAssignment(var, op, value);
            treeData.statementMap.put(ctx, res);
            typeChecks.add(res);
        } catch (UndeclaredVariable e) {
            errors.add(e);
            //TODO what now?
        }
    }

    @Override
    public void exitLocalVariableDeclarationStatement(FrontierParser.LocalVariableDeclarationStatementContext ctx) {
        try {
            FVarDeclaration res = fromVariableDeclarator(ctx.variableDeclarator());
            declaredVars.put(res.getVar().getIdentifier(), (FLocalVariable) res.getVar());
            treeData.statementMap.put(ctx, res);
        } catch (ClassNotFound e) {
            errors.add(e);
            //TODO what now?
        }
    }

    @Override
    public void enterBlockStatement(FrontierParser.BlockStatementContext ctx) {
        declaredVars.push();
    }
    @Override
    public void exitBlockStatement(FrontierParser.BlockStatementContext ctx) {
        declaredVars.pop();
        FBlock res = new FBlock(statementsFromList(ctx.statement()));
        treeData.statementMap.put(ctx, res);
    }

    @Override
    public void exitIfStatement(FrontierParser.IfStatementContext ctx) {
        FExpression cond = treeData.expressionMap.get(ctx.expression());
        FStatement then = treeData.statementMap.get(ctx.statement(0));
        FStatement elze = treeData.statementMap.get(ctx.statement(1));
        FIf res = new FIf(cond, then, elze);
        treeData.statementMap.put(ctx, res);
        typeChecks.add(res);
    }

    @Override
    public void enterWhileStatement(FrontierParser.WhileStatementContext ctx) {
        declaredVars.push();
        loops.push(new FLoopIdentifier());
    }
    @Override
    public void exitWhileStatement(FrontierParser.WhileStatementContext ctx) {
        declaredVars.pop();
        FLoopIdentifier identifier = loops.pop();
        FExpression cond = treeData.expressionMap.get(ctx.expression());
        FStatement body = treeData.statementMap.get(ctx.statement());
        FWhile res = new FWhile(identifier, cond, body);
        treeData.statementMap.put(ctx, res);
        typeChecks.add(res);
    }

    @Override
    public void enterForStatement(FrontierParser.ForStatementContext ctx) {
        declaredVars.push();
        loops.push(new FLoopIdentifier());
    }
    @Override
    public void exitForStatement(FrontierParser.ForStatementContext ctx) {
        declaredVars.pop();
        FLoopIdentifier identifier = loops.pop();

        FVarDeclaration decl = null;
        FrontierParser.VariableDeclaratorContext c = ctx.variableDeclarator();
        if (c != null) {
            try {
                decl = fromVariableDeclarator(c);
            } catch (ClassNotFound e) {
                errors.add(e);
                return; //TODO what now?
            }
        }

        FExpression cond = treeData.expressionMap.get(ctx.expression());
        FExpression inc = treeData.expressionMap.get(ctx.expression2().expression());
        FStatement body = treeData.statementMap.get(ctx.statement());

        FFor res = new FFor(identifier, decl, cond, inc, body);
        treeData.statementMap.put(ctx, res);
        typeChecks.add(res);
    }

    @Override
    public void enterForeachStatement(FrontierParser.ForeachStatementContext ctx) {
        declaredVars.push();
        loops.push(new FLoopIdentifier());
    }
    @Override
    public void exitForeachStatement(FrontierParser.ForeachStatementContext ctx) {
        super.exitForeachStatement(ctx);
        declaredVars.pop();
        FLoopIdentifier identifier = loops.pop();

        FLocalVariable it;
        try {
            it = ParserContextUtils.getVariable(ctx.typedIdentifier(), file.getClasses());
        } catch (ClassNotFound e) {
            errors.add(e);
            return; //TODO now what?
        }

        FExpression container = treeData.expressionMap.get(ctx.expression());
        FStatement body = treeData.statementMap.get(ctx.statement());

        FForEach res = new FForEach(identifier, it, container, body);
        treeData.statementMap.put(ctx, res);
        typeChecks.add(res);
    }

    @Override
    public void exitBreakStatement(FrontierParser.BreakStatementContext ctx) {
        if (loops.isEmpty()) {
            errors.add(new StatementOutsideLoop());
            //TODO what now
            return;
        }
        treeData.statementMap.put(ctx, new FBreak(loops.peek()));
    }

    @Override
    public void exitContinueStatement(FrontierParser.ContinueStatementContext ctx) {
        if (loops.isEmpty()) {
            errors.add(new StatementOutsideLoop());
            //TODO what now
            return;
        }
        treeData.statementMap.put(ctx, new FContinue(loops.peek()));
    }


    //Expressions

    @Override
    public void exitLiteralExpr(FrontierParser.LiteralExprContext ctx) {
        FLiteral literal = treeData.literalMap.get(ctx.literal());
        treeData.expressionMap.put(ctx, new FLiteralExpression(literal));
    }

    @Override
    public void exitThisExpr(FrontierParser.ThisExprContext ctx) {
        FVariableIdentifier identifier = FVariableIdentifier.THIS;
        try {
            treeData.expressionMap.put(ctx, new FVariableExpression(findVar(identifier)));
        } catch (UndeclaredVariable e) {
            //TODO maybe abort the analysis of the current expression but continue from there on?
        }
    }

    @Override
    public void exitVariableExpr(FrontierParser.VariableExprContext ctx) {
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        try {
            treeData.expressionMap.put(ctx, new FVariableExpression(findVar(identifier)));
        } catch (UndeclaredVariable e) {
            //TODO abort
        }
    }

    @Override
    public void exitBracketsExpr(FrontierParser.BracketsExprContext ctx) {
        FExpression inner = treeData.expressionMap.get(ctx.expression());
        treeData.expressionMap.put(ctx, new FBracketsExpression(inner));
    }

    @Override
    public void exitPreUnaryOp(FrontierParser.PreUnaryOpContext ctx) {
        FExpression expression = treeData.expressionMap.get(ctx.expression());
        FFunctionIdentifier identifier = Operators.PreUnary.fromString(ctx.getChild(0).getText()).identifier;

        try {
            FFunctionCall function = staticFunctionCall(expression.getType(), identifier, ImmutableList.of(expression));
            treeData.expressionMap.put(ctx, function);
        } catch (FunctionNotFound | StaticAccessToInstanceFunction e) {
            errors.add(e);
            return; //TODO abort
        }
    }

    @Override
    public void exitBinaryOp(FrontierParser.BinaryOpContext ctx) {
        FExpression first = treeData.expressionMap.get(ctx.expression(0));
        FExpression second = treeData.expressionMap.get(ctx.expression(1));
        FFunctionIdentifier identifier = Operators.Binary.fromString(ctx.getChild(1).getText()).identifier;

        try {
            FFunctionCall function = staticFunctionCall(first.getType(), identifier, ImmutableList.of(first, second));
            treeData.expressionMap.put(ctx, function);
        } catch (FunctionNotFound | StaticAccessToInstanceFunction e1) {
            try {
                FFunctionCall function = staticFunctionCall(second.getType(), identifier, ImmutableList.of(first, second));
                treeData.expressionMap.put(ctx, function);
            } catch (FunctionNotFound | StaticAccessToInstanceFunction e2) {
                errors.add(e1);
                errors.add(e2);
                return; //TODO abort
            }
        }
    }

    @Override
    public void exitArrayAccess(FrontierParser.ArrayAccessContext ctx) {
        FExpression array = treeData.expressionMap.get(ctx.expression(0));
        FExpression index = treeData.expressionMap.get(ctx.expression(1));
        try {
            FArrayAccess res = new FArrayAccess(array, index);
            treeData.expressionMap.put(ctx, res);
        } catch (IncompatibleTypes e) {
            errors.add(e);
            return; //TODO abort
        }
    }

    @Override
    public void exitFieldAccess(FrontierParser.FieldAccessContext ctx) {
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        FExpression object = treeData.expressionMap.get(ctx.expression());
        FField f = object.getType().getField(identifier);
        if (f == null) {
            errors.add(new FieldNotFound(identifier));
            return; //TODO abort
        }
        treeData.expressionMap.put(ctx, new FFieldAccess(f, object));
    }

    @Override
    public void exitStaticFieldAccess(FrontierParser.StaticFieldAccessContext ctx) {
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        try {
            FClass clazz = ParserContextUtils.getType(ctx.typeType(), file.getClasses());
            FField f = clazz.getField(identifier);
            if (f == null) {
                errors.add(new FieldNotFound(identifier));
                return; //TODO abort
            }
            treeData.expressionMap.put(ctx, new FFieldAccess(f));
        } catch (ClassNotFound | StaticAccessToInstanceField e) {
            errors.add(e);
            return; //TODO abort
        }
    }

    private List<FExpression> getExpressions (FrontierParser.ExpressionListContext ctx) {
        if (ctx == null)
            return Collections.emptyList();
        List<FrontierParser.ExpressionContext> cs = ctx.expression();
        List<FExpression> res = new ArrayList<>(cs.size());
        for (FrontierParser.ExpressionContext c : cs)
            res.add(treeData.expressionMap.get(c));
        return res;
    }
    private List<FClass> typesFromExpressionList (List<FExpression> exps) {
        List<FClass> res = new ArrayList<>(exps.size());
        for (FExpression exp : exps)
            res.add(exp.getType());
        return res;
    }
    private FFunctionCall functionCall (FExpression object,
                                        FFunctionIdentifier identifier,
                                        List<FExpression> params)
            throws FunctionNotFound {
        List<FClass> paramTypes = typesFromExpressionList(params);
        FFunction.Signature signature = new FFunction.Signature(identifier, paramTypes);
        FFunction f = object.getType().getFunction(signature);
        if (f==null)
            throw new FunctionNotFound(signature);
        return new FFunctionCall(object, f, params);
    }

    private FFunctionCall staticFunctionCall (FClass clazz,
                                              FFunctionIdentifier identifier,
                                              List<FExpression> params)
            throws FunctionNotFound, StaticAccessToInstanceFunction {
        List<FClass> paramTypes = typesFromExpressionList(params);
        FFunction.Signature signature = new FFunction.Signature(identifier, paramTypes);
        FFunction f = clazz.getFunction(signature);
        if (f==null)
            throw new FunctionNotFound(signature);
        return new FFunctionCall(f, params);
    }

    @Override
    public void exitExternalFunctionCall(FrontierParser.ExternalFunctionCallContext ctx) {
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
        FExpression object = treeData.expressionMap.get(ctx.expression());

        try {
            FFunctionCall res = functionCall(object, identifier, getExpressions(ctx.expressionList()));
            treeData.expressionMap.put(ctx, res);
        } catch (FunctionNotFound e) {
            errors.add(e);
            //TODO abort
        }
    }

    @Override
    public void exitInternalFunctionCall(FrontierParser.InternalFunctionCallContext ctx) {
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
        try {
            FFunctionCall res = functionCall(new FVariableExpression(currentClass.getThis()), identifier, getExpressions(ctx.expressionList()));
            treeData.expressionMap.put(ctx, res);
        } catch (FunctionNotFound e) {
            errors.add(e);
            return; //TODO abort
        }
    }

    @Override
    public void exitStaticFunctionCall(FrontierParser.StaticFunctionCallContext ctx) {
        try {
            FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
            FClass clazz = ParserContextUtils.getType(ctx.typeType(), file.getClasses());
            FFunctionCall res = staticFunctionCall(clazz, identifier, getExpressions(ctx.expressionList()));
            treeData.expressionMap.put(ctx, res);
        } catch (ClassNotFound | FunctionNotFound | StaticAccessToInstanceFunction e) {
            errors.add(e);
            return; //TODO abort
        }
    }

    @Override
    public void exitNewObject(FrontierParser.NewObjectContext ctx) {
        try {
            FClass clazz = ParserContextUtils.getBasicType(ctx.basicType(), file.getClasses());
            FFunctionCall res = staticFunctionCall(clazz, FFunctionIdentifier.CONSTRUCTOR, getExpressions(ctx.expressionList()));
            treeData.expressionMap.put(ctx, res);
        } catch (ClassNotFound |FunctionNotFound | StaticAccessToInstanceFunction e) {
            errors.add(e);
            return; //TODO abort
        }

    }

    @Override
    public void exitNewArray(FrontierParser.NewArrayContext ctx) {
        FClass baseClass;
        try {
            baseClass = ParserContextUtils.getBasicType(ctx.basicType(), file.getClasses());
        } catch (ClassNotFound e) {
            errors.add(e);
            return; //TODO abort
        }

        List<FrontierParser.ExpressionContext> cs = ctx.expression();
        int initialisedDepth = cs.size();
        int uninitialisedDepth = ctx.Array().size();
        FArray array = FArray.getArrayFrom(baseClass, initialisedDepth+uninitialisedDepth);

        List<FExpression> params = new ArrayList<>(initialisedDepth);
        for (FrontierParser.ExpressionContext c : cs)
            params.add(treeData.expressionMap.get(c));

        try {
            FFunctionCall res = staticFunctionCall(array, FFunctionIdentifier.CONSTRUCTOR, params);
            treeData.expressionMap.put(ctx, res);
        } catch (FunctionNotFound | StaticAccessToInstanceFunction e) {
            errors.add(e);
            return; //TODO abort
        }
    }

    //literals
    //TODO move literal processing steps from lexer to parser to properly deal with them
    //TODO store original representation as well to be able to reconstruct it
    @Override
    public void exitLiteral(FrontierParser.LiteralContext ctx) {
        treeData.literalMap.put(ctx, ParserContextUtils.getLiteral(ctx));
    }
}

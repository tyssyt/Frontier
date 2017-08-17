package tys.frontier.parser.syntaxTree;

import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.FClass;
import tys.frontier.code.FConstructor;
import tys.frontier.code.FFunction;
import tys.frontier.code.FVariable;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.parser.FrontierBaseListener;
import tys.frontier.parser.FrontierParser;
import tys.frontier.parser.syntaxTree.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxTree.syntaxErrors.UndeclaredVariable;
import tys.frontier.util.MapStack;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ToInternalRepresentation extends FrontierBaseListener {

    private Map<FClassIdentifier, FClass> classes;
    private SyntaxTreeData treeData;
    private List<SyntaxError> errors = new ArrayList<>();
    private FClass currentClass;
    private FFunction currentFunction;
    private MapStack<FVariableIdentifier, FVariable> declaredVars = new MapStack<>();


    private ToInternalRepresentation(Map<FClassIdentifier, FClass> classes, SyntaxTreeData syntaxTreeData) {
        this.classes = classes;
        this.treeData = syntaxTreeData;
    }

    public static List<SyntaxError> toInternal (Map<FClassIdentifier, FClass> classes, SyntaxTreeData syntaxTreeData) {
        ToInternalRepresentation listener = new ToInternalRepresentation(classes, syntaxTreeData);
        ParseTreeWalker.DEFAULT.walk(listener, syntaxTreeData.root);
        return listener.errors;
    }

    @Override
    public void enterClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentClass = treeData.classes.get(ctx);
    }

    //fields

    //methods enter & exit
    @Override
    public void enterMethodDeclaration(FrontierParser.MethodDeclarationContext ctx) {
        FFunction f = treeData.functions.get(ctx);
        currentFunction = f;
        declaredVars.push(Utils.asMap(f.getParams()));
        if (!f.isStatic()) {
            FVariable thiz = currentClass.getThis();
            declaredVars.put(thiz.getIdentifier(), thiz);
        }
    }

    @Override
    public void exitMethodDeclaration(FrontierParser.MethodDeclarationContext ctx) {
        currentFunction = null;
        declaredVars.pop();
    }

    @Override
    public void enterConstructorDeclaration(FrontierParser.ConstructorDeclarationContext ctx) {
        FConstructor c = treeData.constructors.get(ctx);
        declaredVars.push(Utils.asMap(c.getParams()));
        FVariable thiz = currentClass.getThis();
        declaredVars.put(thiz.getIdentifier(), thiz);
    }

    @Override
    public void exitConstructorDeclaration(FrontierParser.ConstructorDeclarationContext ctx) {
        declaredVars.pop();
    }

    //statements

    //Expressions

    @Override
    public void exitLiteralExpr(FrontierParser.LiteralExprContext ctx) {
        FLiteral literal = treeData.literalMap.get(ctx.literal());
        treeData.expressionMap.put(ctx, new FLiteralExpression(literal));
    }

    @Override
    public void exitThisExpr(FrontierParser.ThisExprContext ctx) {
        FVariableIdentifier identifier = FVariableIdentifier.THIS;
        FVariable var = declaredVars.get(identifier);
        if (var == null) {
            errors.add(new UndeclaredVariable(identifier, currentFunction));
        }
        treeData.expressionMap.put(ctx, new FVariableExpression(var));
    }

    @Override
    public void exitVariableExpr(FrontierParser.VariableExprContext ctx) {
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        FVariable var = declaredVars.get(identifier);
        if (var == null) {
            errors.add(new UndeclaredVariable(identifier, currentFunction));
        }
        treeData.expressionMap.put(ctx, new FVariableExpression(var));
    }

    @Override
    public void exitBracketsExpr(FrontierParser.BracketsExprContext ctx) {
        FExpression inner = treeData.expressionMap.get(ctx.expression());
        treeData.expressionMap.put(ctx, new FBracketsExpression(inner));
    }

    @Override
    public void exitPreUnaryOp(FrontierParser.PreUnaryOpContext ctx) {
        //TODO type check
        FExpression expression = treeData.expressionMap.get(ctx.children.get(1));
        FUnaryOp.Operator op = FUnaryOp.Operator.fromString(ctx.children.get(0).getText(), true);
        FUnaryOp res = new FUnaryOp(expression, op);
        treeData.expressionMap.put(ctx, res);
    }

    @Override
    public void exitPostUnaryOp(FrontierParser.PostUnaryOpContext ctx) {
        //TODO type check
        FExpression expression = treeData.expressionMap.get(ctx.children.get(0));
        FUnaryOp.Operator op = FUnaryOp.Operator.fromString(ctx.children.get(1).getText(), false);
        FUnaryOp res = new FUnaryOp(expression, op);
        treeData.expressionMap.put(ctx, res);
    }

    @Override
    public void exitBinaryOp(FrontierParser.BinaryOpContext ctx) {
        //TODO type check
        FExpression first = treeData.expressionMap.get(ctx.children.get(0));
        FExpression second = treeData.expressionMap.get(ctx.children.get(2));
        FBinaryOp.Operator op = FBinaryOp.Operator.fromString(ctx.children.get(1).getText());
        FBinaryOp res = new FBinaryOp(first, second, op);
        treeData.expressionMap.put(ctx, res);
    }

    @Override
    public void exitNewExpr(FrontierParser.NewExprContext ctx) {
        super.exitNewExpr(ctx);
    }

    @Override
    public void exitMemberAccess(FrontierParser.MemberAccessContext ctx) {
        super.exitMemberAccess(ctx);
    }

    @Override
    public void exitArrayAccess(FrontierParser.ArrayAccessContext ctx) {
        super.exitArrayAccess(ctx);
    }

    @Override
    public void exitMethodCall(FrontierParser.MethodCallContext ctx) {
        super.exitMethodCall(ctx);
    }

    //literals

    //TODO move literal processing steps from lexer to parser to properly deal with them
    //TODO store original representation as well to be able to reconstruct it
    @Override
    public void exitLiteral(FrontierParser.LiteralContext ctx) {
        treeData.literalMap.put(ctx, ParserContextUtils.getLiteral(ctx));
    }

    //TODO -------------------------

    @Override
    public void exitCreator(FrontierParser.CreatorContext ctx) {
        super.exitCreator(ctx);
    }

    @Override
    public void exitCreatedName(FrontierParser.CreatedNameContext ctx) {
        super.exitCreatedName(ctx);
    }

    @Override
    public void exitArrayCreatorRest(FrontierParser.ArrayCreatorRestContext ctx) {
        super.exitArrayCreatorRest(ctx);
    }

    @Override
    public void exitClassCreatorRest(FrontierParser.ClassCreatorRestContext ctx) {
        super.exitClassCreatorRest(ctx);
    }

    @Override
    public void exitPrimitiveType(FrontierParser.PrimitiveTypeContext ctx) {
        super.exitPrimitiveType(ctx);
    }

    @Override
    public void visitTerminal(TerminalNode node) {
        super.visitTerminal(node);
    }
}

package tys.frontier.parser.syntaxTree;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import tys.frontier.code.*;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.parser.FrontierBaseListener;
import tys.frontier.parser.FrontierParser;
import tys.frontier.parser.syntaxTree.syntaxErrors.*;
import tys.frontier.util.MapStack;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;

public class ToInternalRepresentation extends FrontierBaseListener {

    private Map<FClassIdentifier, FClass> classes;
    private SyntaxTreeData treeData;
    private List<SyntaxError> errors = new ArrayList<>();
    private FClass currentClass;
    private FFunction currentFunction;
    private MapStack<FVariableIdentifier, FLocalVariable> declaredVars = new MapStack<>();


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
            FLocalVariable thiz = currentClass.getThis();
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
        FLocalVariable thiz = currentClass.getThis();
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
    public void exitArrayAccess(FrontierParser.ArrayAccessContext ctx) {
        super.exitArrayAccess(ctx);
    }

    @Override
    public void exitFieldAccess(FrontierParser.FieldAccessContext ctx) {
        FVariableIdentifier identifier = new FVariableIdentifier(ctx.Identifier().getText());
        FClass clazz = treeData.expressionMap.get(ctx.expression()).getType();
        FField f = clazz.getField(identifier);
        if (f == null)
            errors.add(new FieldNotFound(identifier)); //TODO abort
        treeData.expressionMap.put(ctx, new FFieldAccess(f));
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
    private Multiset<FClass> typesFromExpressionList (List<FExpression> exps) {
        Multiset<FClass> res = HashMultiset.create();
        for (FExpression exp : exps)
            res.add(exp.getType());
        return res;
    }
    private FFunctionCall functionCall (FFunctionIdentifier identifier,
                                        List<FExpression> params,
                                        FClass clazz)
                                    throws FunctionNotFound {
        Multiset<FClass> paramTypes = typesFromExpressionList(params);
        FFunction.Signature signature = new FFunction.Signature(identifier, paramTypes);
        FFunction f = clazz.getFunction(signature);
        if (f==null)
            throw new FunctionNotFound(signature);
        return new FFunctionCall(f, params);
    }

    @Override
    public void exitExternalFunctionCall(FrontierParser.ExternalFunctionCallContext ctx) {
        FFunctionIdentifier identifier = new FFunctionIdentifier(ctx.Identifier().getText());
        FClass clazz = treeData.expressionMap.get(ctx.expression()).getType();

        try {
            FFunctionCall res = functionCall(identifier, getExpressions(ctx.expressionList()), clazz);
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
            FFunctionCall res = functionCall(identifier, getExpressions(ctx.expressionList()), currentClass);
            treeData.expressionMap.put(ctx, res);
        } catch (FunctionNotFound e) {
            errors.add(e);
            //TODO abort
        }
    }

    @Override
    public void exitNewObject(FrontierParser.NewObjectContext ctx) {
        Pair<FClass, Optional<ClassNotFound>> clazz = ParserContextUtils.getBasicType(ctx.basicType(), classes);
        if (clazz.b.isPresent()) {
            errors.add(clazz.b.get());
            //TODO abort
        }

        try {
            FFunctionCall res = functionCall(FFunctionIdentifier.CONSTRUCTOR, getExpressions(ctx.expressionList()), clazz.a);
            treeData.expressionMap.put(ctx, res);
        } catch (FunctionNotFound e) {
            errors.add(e);
            //TODO abort
        }

    }

    @Override
    public void exitNewArray(FrontierParser.NewArrayContext ctx) {
        Pair<FClass, Optional<ClassNotFound>> baseClass = ParserContextUtils.getBasicType(ctx.basicType(), classes);
        if (baseClass.b.isPresent()) {
            errors.add(baseClass.b.get());
            //TODO abort
        }

        int initialisedDepth = ctx.LBRACK().size();
        int uninitialisedDepth = ctx.Array().size();
        FArray array = FArray.getArrayFrom(baseClass.a, initialisedDepth+uninitialisedDepth);

        List<FrontierParser.ExpressionContext> cs = ctx.expression();
        List<FExpression> params = new ArrayList<>(cs.size());
        for (FrontierParser.ExpressionContext c : cs)
            params.add(treeData.expressionMap.get(c));

        try {
            FFunctionCall res = functionCall(FFunctionIdentifier.CONSTRUCTOR, params, array);
            treeData.expressionMap.put(ctx, res);
        } catch (FunctionNotFound e) {
            errors.add(e);
            //TODO abort
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

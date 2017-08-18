package tys.frontier.parser.syntaxTree;

import tys.frontier.code.FClass;
import tys.frontier.code.FConstructor;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.statement.FStatement;
import tys.frontier.parser.FrontierParser;

import java.util.HashMap;
import java.util.Map;

public class SyntaxTreeData {

    //First Pass: Global Identifiers
    public final FrontierParser.FileContext root;
    public final Map<FrontierParser.ClassDeclarationContext, FClass> classes = new HashMap<>();
    public final Map<FrontierParser.FieldDeclarationContext, FField> fields = new HashMap<>();
    public final Map<FrontierParser.MethodDeclarationContext, FFunction> functions = new HashMap<>();
    public final Map<FrontierParser.ConstructorDeclarationContext, FConstructor> constructors = new HashMap<>();

    //Second pass: literals, Expressions, Statements
    public final Map<FrontierParser.LiteralContext, FLiteral> literalMap = new HashMap<>();
    public final Map<FrontierParser.ExpressionContext, FExpression> expressionMap = new HashMap<>();
    public final Map<FrontierParser.StatementContext, FStatement> statementMap = new HashMap<>();

    public SyntaxTreeData(FrontierParser.FileContext root) {
        this.root = root;
    }
}

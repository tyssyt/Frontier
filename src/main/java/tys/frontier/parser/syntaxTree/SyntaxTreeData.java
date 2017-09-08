package tys.frontier.parser.syntaxTree;

import tys.frontier.code.FClass;
import tys.frontier.code.FConstructor;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
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

    public SyntaxTreeData(FrontierParser.FileContext root) {
        this.root = root;
    }
}

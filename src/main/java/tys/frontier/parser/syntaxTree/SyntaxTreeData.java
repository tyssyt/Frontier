package tys.frontier.parser.syntaxTree;

import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.parser.antlr.FrontierParser;

import java.util.HashMap;
import java.util.Map;

public class SyntaxTreeData {

    //First Pass: Global Identifiers
    public final FrontierParser.FileContext root;
    public final Map<FrontierParser.ClassDeclarationContext, DefaultNamespace> classNamespaces = new HashMap<>();
    public final Map<FrontierParser.NamespaceDeclarationContext, DefaultNamespace> namespaces = new HashMap<>();
    public final Map<FrontierParser.FieldDeclarationContext, FField> fields = new HashMap<>();
    public final Map<FrontierParser.MethodHeaderContext, FFunction> functions = new HashMap<>();
    public final Map<FrontierParser.FormalParameterContext, FParameter> parameters = new HashMap<>();

    public SyntaxTreeData(FrontierParser.FileContext root) {
        this.root = root;
    }
}

package tys.frontier.parser.syntaxErrors;

public class UnresolvableImport extends SyntaxError {

    private String moduleName;

    public UnresolvableImport(String moduleName) {
        super(moduleName);
        this.moduleName = moduleName;
    }
}

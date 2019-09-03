package tys.frontier.parser.syntaxErrors;

import java.io.IOException;

public class UnresolvableImport extends SyntaxError {

    private String moduleName;

    public UnresolvableImport(String moduleName, IOException cause) {
        super(moduleName, cause);
        this.moduleName = moduleName;
    }
}

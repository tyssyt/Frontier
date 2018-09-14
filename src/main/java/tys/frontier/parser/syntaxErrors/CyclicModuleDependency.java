package tys.frontier.parser.syntaxErrors;

public class CyclicModuleDependency extends SyntaxError {

    public final String module;

    public CyclicModuleDependency(String module) {
        super("Cyclic Module depency: " + module);
        this.module = module;
    }
}

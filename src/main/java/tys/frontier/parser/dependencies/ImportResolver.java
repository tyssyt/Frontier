package tys.frontier.parser.dependencies;

import tys.frontier.code.module.Module;
import tys.frontier.parser.Parser;
import tys.frontier.parser.syntaxErrors.CyclicModuleDependency;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.parser.syntaxErrors.UnresolvableImport;
import tys.frontier.style.Style;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ImportResolver {

    private Style style;
    private Map<String, Module> resolvedModules = new HashMap<>();
    private Set<String> queuedRequests = new HashSet<>();

    public ImportResolver(Style style) {
        this.style = style;
    }

    Module requestModule(String name) throws UnresolvableImport, CyclicModuleDependency, SyntaxErrors {
        Module res = resolvedModules.get(name);
        if (res == null) {
            try {
                if (queuedRequests.contains(name))
                    throw new CyclicModuleDependency(name);
                queuedRequests.add(name);
                res = new Parser(name, style, this).parse(); //TODO are the libs present in the user style, are other files?
            } catch (IOException e) {
                throw new UnresolvableImport(name, e);
            }

            resolvedModules.put(name, res);
            queuedRequests.remove(name);
        }
        return res;
    }


}

package tys.frontier.parser.modules;

import tys.frontier.code.module.Module;
import tys.frontier.parser.Parser;
import tys.frontier.parser.syntaxErrors.CyclicModuleDependency;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.parser.syntaxErrors.UnresolvableImport;

import java.io.IOException;
import java.nio.file.Path;
import java.util.*;

public class ImportResolver {

    private List<ModuleRepository> repositories;

    private Map<String, Module> resolvedModules = new HashMap<>();
    private Set<String> queuedRequests = new HashSet<>();

    public ImportResolver(List<ModuleRepository> repositories) {
        this.repositories = repositories;
    }

    public Module requestModule(String name) throws SyntaxError, SyntaxErrors {
        Module res = resolvedModules.get(name);
        if (res == null) {
            try {
                if (queuedRequests.contains(name))
                    throw new CyclicModuleDependency(name);
                queuedRequests.add(name);

                for (ModuleRepository repository : repositories) {
                    Path path = repository.resolve(name);
                    if (path != null) {
                        res = Parser.parse(path, repository.getStyle());
                        resolvedModules.put(name, res);
                        queuedRequests.remove(name);
                        return res;
                    }
                }
                throw new UnresolvableImport(name, null);
            } catch (IOException e) {
                throw new UnresolvableImport(name, e);
            }
        }
        return res;
    }


}

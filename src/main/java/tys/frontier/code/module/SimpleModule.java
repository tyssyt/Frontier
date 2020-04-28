package tys.frontier.code.module;

import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

public class SimpleModule implements Module {

    private Map<FIdentifier, DefaultNamespace> exportedNamespaces;
    private List<Module> imports;
    private List<Path> nativeIncludes;

    public SimpleModule(Map<FIdentifier, DefaultNamespace> exportedNamespaces, List<Module> imports, List<Path> nativeIncludes) {
        this.exportedNamespaces = exportedNamespaces;
        this.imports = imports;
        this.nativeIncludes = nativeIncludes;
    }

    @Override
    public FFunction findMain() throws IllegalArgumentException, NoSuchElementException {
        throw new NoSuchElementException();
    }

    @Override
    public Map<FIdentifier, DefaultNamespace> getExportedNamespaces() {
        return exportedNamespaces;
    }

    @Override
    public DefaultNamespace getNamespace(FIdentifier identifier) {
        return exportedNamespaces.get(identifier);
    }

    @Override
    public Stream<DefaultNamespace> getNamespaces() {
        return exportedNamespaces.values().stream();
    }

    @Override
    public List<Path> getNativeIncludes() {
        return nativeIncludes;
    }

    @Override
    public List<Module> getImports() {
        return imports;
    }
}

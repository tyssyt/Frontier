package tys.frontier.code.module;

import com.google.common.base.Suppliers;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.google.common.collect.MoreCollectors.onlyElement;
import static java.util.stream.Collectors.toMap;
import static tys.frontier.util.Utils.asMap;

public class FrontierModule implements Module {

    private String name;
    private List<Module> imports;
    private List<Path> files;
    private List<Include> nativeIncludes;

    private Map<FIdentifier, DefaultNamespace> namespaces;

    //cached thingies
    private Supplier<Map<FIdentifier, DefaultNamespace>> exportedNamespaces = Suppliers.memoize(this::initExportedNamespaces);

    public FrontierModule(String name, List<Module> imports, List<Path> files, List<Include> nativeIncludes, List<DefaultNamespace> namespaces) {
        this.name = name;
        this.imports = imports;
        this.files = files;
        this.nativeIncludes = nativeIncludes;
        this.namespaces = asMap(namespaces);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public List<Module> getImports() {
        return imports;
    }

    @Override
    public List<Include> getNativeIncludes() {
        return nativeIncludes;
    }

    public List<Path> getFiles() {
        return files;
    }

    @Override
    public Map<FIdentifier, DefaultNamespace> getNamespaces() {
        return namespaces;
    }

    @Override
    public Map<FIdentifier, DefaultNamespace> getExportedNamespaces() {
        return exportedNamespaces.get();
    }
    private Map<FIdentifier, DefaultNamespace> initExportedNamespaces() {
        return namespaces.values().stream()
                .filter(namespace -> namespace.getVisibility() == FVisibilityModifier.EXPORT)
                .collect(toMap(DefaultNamespace::getIdentifier, Function.identity()));
    }

    public FFunction findMain() throws IllegalArgumentException, NoSuchElementException {
        return namespaces.values().stream()
                .flatMap(namespace -> namespace.getFunctions(false).values().stream())
                .map(Signature::getFunction)
                .filter(FFunction::isMain)
                .collect(onlyElement());
    }
}
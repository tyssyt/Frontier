package tys.frontier.code.module;

import com.google.common.collect.MoreCollectors;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.parser.ParsedFile;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toMap;

public class FrontierModule implements Module {

    private ParsedFile entryPoint;
    private List<Include> nativeIncludes = new ArrayList<>();

    //cached thingies
    private Map<FIdentifier, DefaultNamespace> exportedNamespaces;
    private List<ParsedFile> files;

    public ParsedFile getEntryPoint() {
        return entryPoint;
    }

    @Override
    public FFunction findMain() throws IllegalArgumentException, NoSuchElementException {
        return getExportedNamespaces().values().stream()
                .flatMap(namespace -> namespace.getFunctions(false).values().stream())
                .map(Signature::getFunction)
                .filter(FFunction::isMain)
                .collect(MoreCollectors.onlyElement());
    }

    public void setEntryPoint(ParsedFile entryPoint) {
        assert this.entryPoint == null;
        this.entryPoint = entryPoint;
    }

    @Override
    public Map<FIdentifier, DefaultNamespace> getExportedNamespaces() {
        if (exportedNamespaces == null)
            exportedNamespaces = initExportedNamespaces();
        return exportedNamespaces;
    }

    private Map<FIdentifier, DefaultNamespace> initExportedNamespaces() {
        return getNamespaces()
                .filter(namespace -> namespace.getVisibility() == FVisibilityModifier.EXPORT)
                .collect(toMap(DefaultNamespace::getIdentifier, Function.identity()));
    }

    @Override
    public DefaultNamespace getNamespace(FIdentifier identifier) {
        for (ParsedFile file : getFiles()) {
            DefaultNamespace namespace = file.getNamespaces().get(identifier);
            if (namespace != null && namespace.getVisibility() != FVisibilityModifier.PRIVATE)
                return namespace;
        }
        return null;
    }

    @Override
    public Stream<DefaultNamespace> getNamespaces() {
        return getFiles().stream().flatMap(file -> file.getNamespaces().values().stream());
    }

    public List<ParsedFile> getFiles() {
        if (files == null)
            files = initFiles();
        return files;
    }

    private List<ParsedFile> initFiles()  {
        List<ParsedFile> res = new ArrayList<>();
        Queue<ParsedFile> toDo = new ArrayDeque<>();
        toDo.add(entryPoint);
        while (!toDo.isEmpty()) {
            ParsedFile cur = toDo.remove();
            res.add(cur);
            toDo.addAll(cur.getIncludes());
        }
        return res;
    }

    @Override
    public List<Include> getNativeIncludes() {
        return nativeIncludes;
    }

    public void addNativeIncludes(List<Include> nativeIncludes) {
        this.nativeIncludes.addAll(nativeIncludes);
    }

    @Override
    public List<Module> getImports() {
        List<Module> res = new ArrayList<>();
        for (ParsedFile file : getFiles()) {
            res.addAll(file.getImports());
        }
        return res;
    }
}
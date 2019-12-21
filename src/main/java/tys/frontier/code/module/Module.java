package tys.frontier.code.module;

import com.google.common.collect.MoreCollectors;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.type.FClass;
import tys.frontier.code.visitor.ModuleVisitor;
import tys.frontier.code.visitor.ModuleWalker;
import tys.frontier.parser.ParsedFile;

import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

public class Module {

    private ParsedFile entryPoint;
    private List<Path> nativeIncludes = new ArrayList<>();

    //cached thingies
    private Map<FTypeIdentifier, FClass> exportedClasses;
    private List<ParsedFile> files;

    public ParsedFile getEntryPoint() {
        return entryPoint;
    }

    public FFunction findMain() throws IllegalArgumentException, NoSuchElementException {
        return getExportedClasses().values().stream()
                .flatMap(_class -> _class.getFunctions(false).values().stream())
                .map(Signature::getFunction)
                .filter(FFunction::isMain)
                .collect(MoreCollectors.onlyElement());
    }

    public void setEntryPoint(ParsedFile entryPoint) {
        assert this.entryPoint == null;
        this.entryPoint = entryPoint;
    }

    public Map<FTypeIdentifier, FClass> getExportedClasses() {
        if (exportedClasses == null)
            exportedClasses = initExportedClasses();
        return exportedClasses;
    }

    private Map<FTypeIdentifier, FClass> initExportedClasses() {
        return getClasses()
                .filter(_class -> _class.getVisibility() == FVisibilityModifier.EXPORT)
                .collect(toMap(FClass::getIdentifier, Function.identity()));
    }

    public FClass getClass(FTypeIdentifier identifier) {
        for (ParsedFile file : getFiles()) {
            FClass _class = file.getClass(identifier);
            if (_class != null && _class.getVisibility() != FVisibilityModifier.PRIVATE)
                return _class;
        }
        return null;
    }

    public Stream<FClass> getClasses() {
        return getFiles().stream().flatMap(file -> file.getClasses().values().stream());
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

    public List<Path> getNativeIncludes() {
        return nativeIncludes;
    }

    public void addNativeIncludes(List<Path> nativeIncludes) {
        this.nativeIncludes.addAll(nativeIncludes);
    }

    public List<Module> findImportedModulesReflexiveTransitive() {
        List<Module> res = new ArrayList<>();
        Queue<Module> toDo = new ArrayDeque<>();
        toDo.add(this);
        while (!toDo.isEmpty()) {
            Module cur = toDo.remove();
            res.add(cur);
            for (ParsedFile file : cur.getFiles()) {
                toDo.addAll(file.getImports());
            }
        }
        return res;
    }

    public <M,C,Fi,Fu,S,E> M accept(ModuleWalker<M,C,Fi,Fu,S,E> walker) {
        return walker.enterModule(this);
    }

    public <M,C,Fi,Fu,S,E> M accept(ModuleVisitor<M,C,Fi,Fu,S,E> visitor) {
        visitor.enterModule(this);
        List<C> cs = getClasses().map(_class -> _class.accept(visitor)).collect(toList());
        return visitor.exitModule(this, cs);
    }
}
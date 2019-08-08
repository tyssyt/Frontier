package tys.frontier.code.module;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ModuleVisitor;
import tys.frontier.code.visitor.ModuleWalker;
import tys.frontier.util.StringBuilderToString;

import java.util.*;

public class Module implements StringBuilderToString {

    protected String name;
    protected String version;
    protected String subversion_or_versionSuffix; //optional
    //TODO some better versioning, maybe make a new subclass just for that

    protected List<Module> importedModules = new ArrayList<>(); //dependencies

    protected FFunction entryPoint = null;

    protected BiMap<FTypeIdentifier, FClass> exportedClasses = HashBiMap.create();
    protected BiMap<FTypeIdentifier, FClass> classes = HashBiMap.create();

    public Module(String name, String version, String subversion_or_versionSuffix) {
        assert Character.isUpperCase(name.charAt(0));
        this.name = name;
        this.version = version;
        this.subversion_or_versionSuffix = subversion_or_versionSuffix;
    }

    public String getName() {
        return name;
    }

    public String getVersion() {
        return version;
    }

    public String getSubversion_or_versionSuffix() {
        return subversion_or_versionSuffix;
    }

    public List<Module> getImportedModules() {
        return importedModules;
    }

    public Set<Module> getImportedModulesReflexiveTransitive() {
        Set<Module> res = new HashSet<>();
        Queue<Module> todo = new ArrayDeque<>();
        todo.add(this);
        while (!todo.isEmpty()) {
            Module cur = todo.remove();
            if (res.contains(cur))
                continue;
            res.add(cur);
            todo.addAll(cur.getImportedModules());
        }
        return res;
    }

    public BiMap<FTypeIdentifier, FClass> getClasses() {
        return classes;
    }

    public Map<FTypeIdentifier, FClass> getExportedClasses() {
        return exportedClasses;
    }

    public void addClass(FClass fClass) {
        classes.put(fClass.getIdentifier(), fClass);
        if (fClass.getVisibility() == FVisibilityModifier.EXPORT)
            exportedClasses.put(fClass.getIdentifier(), fClass);
    }

    public Optional<FFunction> getEntryPoint() {
        return Optional.ofNullable(entryPoint);
    }

    public void setEntryPoint(FFunction entryPoint) {
        this.entryPoint = entryPoint;
    }

    public Map<FTypeIdentifier, FType> getImportedClasses () {
        Map<FTypeIdentifier, FType> res = new LinkedHashMap<>();
        for (Module module : importedModules) {
            res.putAll(module.getExportedClasses());
        }
        return res;
    }

    public <M,C,Fi,Fu,S,E> M accept(ModuleWalker<M,C,Fi,Fu,S,E> walker) {
        return walker.enterModule(this);
    }

    public <M,C,Fi,Fu,S,E> M accept(ModuleVisitor<M,C,Fi,Fu,S,E> visitor) {
        visitor.enterModule(this);
        List<C> cs = new ArrayList<>(classes.size());
        for (FClass fClass : classes.values()) {
            cs.add(fClass.accept(visitor));
        }
        return visitor.exitModule(this, cs);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("Module: ").append(name).append(" v").append(version);
        if (subversion_or_versionSuffix != null) {
            sb.append('.').append(subversion_or_versionSuffix);
        }
        if (entryPoint != null) {
            sb.append('\n').append("entry: ").append(entryPoint.headerToString());
        }
        for (FClass fClass : classes.values()) {
            sb.append('\n');
            fClass.summary(sb);
        }
        return sb;
    }

    @Override
    public String toString() {
        return tS();
    }
}
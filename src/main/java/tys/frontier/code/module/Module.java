package tys.frontier.code.module;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FTypeIdentifier;

import java.util.*;

public abstract class Module {

    protected String name;
    protected String version;
    protected String subversion_or_versionSuffix; //optional
    //TODO some better versioning, maybe make a new subclass just for that

    protected List<Module> importedModules = new ArrayList<>(); //dependencies

    protected FFunction entryPoint = null;

    protected BiMap<FTypeIdentifier, FClass> exportedClasses = HashBiMap.create();
    protected Multimap<FClass, FField> exportedFields = HashMultimap.create(); //TODO maybe we just don't need these fields
    protected Multimap<FClass, FFunction> exportedFunctions = HashMultimap.create(); //TODO maybe we just don't need these functions

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

    public Map<FTypeIdentifier, FClass> getExportedClasses() {
        return exportedClasses;
    }

    public Multimap<FClass, FField> getExportedFields() {
        return exportedFields;
    }

    public Multimap<FClass, FFunction> getExportedFunctions() {
        return exportedFunctions;
    }

    public Optional<FFunction> getEntryPoint() {
        return Optional.ofNullable(entryPoint);
    }

    public void setEntryPoint(FFunction entryPoint) {
        this.entryPoint = entryPoint;
    }

    public void addDependency (Module dependency) {
        importedModules.add(dependency);
    }

    public void addDependencies (Collection<Module> dependencies) {
        importedModules.addAll(dependencies);
    }

    public Map<FTypeIdentifier, FClass> getImportedClasses () {
        Map<FTypeIdentifier, FClass> res = new LinkedHashMap<>();
        for (Module module : importedModules) {
            res.putAll(module.getExportedClasses());
        }
        return res;
    }

    protected void addExportedType(FClass toExport) {
        assert toExport.getVisibility() == FVisibilityModifier.EXPORT;
        exportedClasses.put(toExport.getIdentifier(), toExport);
        for (FField field : toExport.getFields()) {
            if (field.getVisibility() != FVisibilityModifier.EXPORT)
                continue;
            exportedFields.put(toExport, field);
        }
        for (FFunction function : toExport.getFunctions()) {
            if (function.getVisibility() != FVisibilityModifier.EXPORT)
                continue;
            exportedFunctions.put(toExport, function);
        }
    }
}
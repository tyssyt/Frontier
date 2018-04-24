package tys.frontier.code.module;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;

import java.util.*;

public abstract class Module {

    protected String name;
    protected String version;
    protected String subversion_or_versionSuffix; //optional
    //TODO some better versioning, maybe make a new subclass just for that

    protected List<Module> importedModules = new ArrayList<>(); //dependencies

    protected Map<FClassIdentifier, FClass> exportedClasses = new HashMap<>();
    protected Multimap<FClass, FField> exportedFields = HashMultimap.create(); //TODO maybe we just don't need these fields
    protected Multimap<FClass, FFunction> exportedFunctions = HashMultimap.create(); //TODO maybe we just don't need these fields

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

    public Map<FClassIdentifier, FClass> getExportedClasses() {
        return exportedClasses;
    }

    public Multimap<FClass, FField> getExportedFields() {
        return exportedFields;
    }

    public Multimap<FClass, FFunction> getExportedFunctions() {
        return exportedFunctions;
    }

    public void addDependency (Module dependency) {
        importedModules.add(dependency);
    }

    public void addDependencies (Collection<Module> dependencies) {
        importedModules.addAll(dependencies);
    }

    public Map<FClassIdentifier, FClass> getImportedClasses () {
        Map<FClassIdentifier, FClass> res = new LinkedHashMap<>();
        for (Module module : importedModules) {
            res.putAll(module.getExportedClasses());
        }
        return res;
    }

    protected void addExportedClass(FClass toExport) {
        assert toExport.getVisibility() == FVisibilityModifier.PUBLIC;
        exportedClasses.put(toExport.getIdentifier(), toExport);
        for (FField field : toExport.getFields().values()) {
            if (field.getModifier() != FVisibilityModifier.PUBLIC)
                continue;
            exportedFields.put(toExport, field);
        }
        for (FFunction function : toExport.getFunctions().values()) {
            if (function.getModifier() != FVisibilityModifier.PUBLIC)
                continue;
            exportedFunctions.put(toExport, function);
        }

    }
}
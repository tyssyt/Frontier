package tys.frontier.code.module;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FVisibilityModifier;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class Module {

    protected String name;
    protected String version;
    protected String subversion_or_versionSuffix; //optional
    //TODO some better versioning, maybe make a new subclass just for that

    protected List<Module> importedModules = new ArrayList<>();

    protected List<FClass> exportedClasses;
    protected Map<FClass, FField> exportedFields = new HashMap<>();
    protected Map<FClass, FFunction> exportedFunctions = new HashMap<>();

    public Module(String name, String version, String subversion_or_versionSuffix, List<FClass> exportedClasses) {
        this.name = name;
        this.version = version;
        this.subversion_or_versionSuffix = subversion_or_versionSuffix;
        this.exportedClasses = exportedClasses;
        for (FClass clazz : exportedClasses) {
            if (clazz.getVisibility() != FVisibilityModifier.PUBLIC)
                continue;
            for (FField field : clazz.getFields().values()) {
                if (field.getModifier() != FVisibilityModifier.PUBLIC)
                    continue;
                exportedFields.put(clazz, field);
            }
            for (FFunction function : clazz.getFunctions().values()) {
                if (function.getModifier() != FVisibilityModifier.PUBLIC)
                    continue;
                exportedFunctions.put(clazz, function);
            }
        }
    }
}
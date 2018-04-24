package tys.frontier.code.module;

import tys.frontier.code.FClass;
import tys.frontier.code.FFile;
import tys.frontier.code.FVisibilityModifier;

import java.util.ArrayList;
import java.util.List;

public class FrontierModule extends Module {

    private List<FFile> files = new ArrayList<>();

    public FrontierModule(String name, String version, String subversion_or_versionSuffix) {
        super(name, version, subversion_or_versionSuffix);
    }

    public List<FFile> getFiles() {
        return files;
    }

    public void addFile (FFile toAdd) {
        files.add(toAdd);
        for (FClass fClass : toAdd.getClasses().values()) {
            if (fClass.getVisibility() == FVisibilityModifier.PUBLIC) {
                addExportedClass(fClass);
            }
        }
    }
}

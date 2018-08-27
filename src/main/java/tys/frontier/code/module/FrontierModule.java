package tys.frontier.code.module;

import tys.frontier.code.FFile;
import tys.frontier.code.FType;
import tys.frontier.code.FVisibilityModifier;

import java.util.ArrayList;
import java.util.List;

public class FrontierModule extends Module {

    private List<FFile> files = new ArrayList<>();

    private ClassHierachy internalClassHierachy = new ClassHierachy();

    public FrontierModule(String name, String version, String subversion_or_versionSuffix) {
        super(name, version, subversion_or_versionSuffix);
    }

    public List<FFile> getFiles() {
        return files;
    }

    public void addFile (FFile toAdd) {
        files.add(toAdd);
        for (FType fType : toAdd.getTypes().values()) {
            internalClassHierachy.add(fType);
            if (fType.getVisibility() == FVisibilityModifier.EXPORT) {
                addExportedType(fType);
            }
        }
    }

    public ClassHierachy getInternalClassHierachy() {
        return internalClassHierachy;
    }
}

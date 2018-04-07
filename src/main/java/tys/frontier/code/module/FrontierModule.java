package tys.frontier.code.module;

import tys.frontier.code.FClass;
import tys.frontier.code.FFile;

import java.util.ArrayList;
import java.util.List;

public class FrontierModule extends Module {

    private List<FFile> files = new ArrayList<>();

    public FrontierModule(String name, String version, String subversion_or_versionSuffix, List<FClass> exportedClasses, List<FFile> files) {
        super(name, version, subversion_or_versionSuffix, exportedClasses);
        this.files = files;
    }

    public static FrontierModule create(String name, String version, String subversion_or_versionSuffix, List<FFile> files) {
        List<FClass> exportedClasses = new ArrayList<>();
        for (FFile file : files) {
            exportedClasses.addAll(file.getClasses().values());
        }
        return new FrontierModule(name, version, subversion_or_versionSuffix, exportedClasses, files);
    }
}

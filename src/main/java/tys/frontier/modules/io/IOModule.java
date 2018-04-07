package tys.frontier.modules.io;

import tys.frontier.code.FClass;
import tys.frontier.code.module.Module;

import java.util.Collections;
import java.util.List;

public class IOModule extends Module {

    public static final IOModule INSTANCE = new IOModule(
            "IO",
            "1.0",
            null,
            Collections.singletonList(IOClass.INSTANCE)
    );

    private IOModule(String name, String version, String subversion_or_versionSuffix, List<FClass> exportedClasses) {
        super(name, version, subversion_or_versionSuffix, exportedClasses);
    }

}

package tys.frontier.modules.io;

import tys.frontier.code.module.Module;

public class IOModule extends Module {

    public static final IOModule INSTANCE = new IOModule(
            "IO",
            "1.0",
            null
    );

    private IOModule(String name, String version, String subversion_or_versionSuffix) {
        super(name, version, subversion_or_versionSuffix);
        addExportedType(IOClass.INSTANCE);
    }

}

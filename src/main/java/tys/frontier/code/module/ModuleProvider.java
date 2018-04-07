package tys.frontier.code.module;

import java.util.Collection;
import java.util.Optional;

public abstract class ModuleProvider {

    abstract Optional<Module> getModule(String name);
    abstract Collection<Module> getModules();

}

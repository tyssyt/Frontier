package tys.frontier.code.visitor;

import tys.frontier.code.module.Module;
import tys.frontier.code.type.FClass;

public interface ModuleWalker<Mod, Class, Field, Function, Statement, Expression> extends ClassWalker<Class, Field, Function, Statement, Expression> {

    default Mod enterModule(Module module) {
        for (FClass fClass : module.getClasses().values()) {
            visitClass(fClass);
        }
        return null;
    }
}

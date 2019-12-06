package tys.frontier.code.visitor;

import tys.frontier.code.module.Module;

public interface ModuleWalker<Mod, Class, Field, Function, Statement, Expression> extends ClassWalker<Class, Field, Function, Statement, Expression> {

    default Mod enterModule(Module module) {
        module.getClasses().forEach(this::visitClass);
        return null;
    }
}

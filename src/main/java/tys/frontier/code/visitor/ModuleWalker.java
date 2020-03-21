package tys.frontier.code.visitor;

import tys.frontier.code.module.Module;

public interface ModuleWalker<Mod, Namespace, Class, Field, Function, Statement, Expression> extends ClassWalker<Class, Namespace, Field, Function, Statement, Expression> {

    default Mod enterModule(Module module) {
        module.getNamespaces().forEach(this::visitNamespace);
        return null;
    }
}

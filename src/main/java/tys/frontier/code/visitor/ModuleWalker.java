package tys.frontier.code.visitor;

import tys.frontier.code.module.Module;
import tys.frontier.code.namespace.DefaultNamespace;

public interface ModuleWalker<Mod, Namespace, Class, Field, Function, Statement, Expression> extends ClassWalker<Class, Namespace, Field, Function, Statement, Expression> {

    default Mod enterModule(Module module) {
        for (DefaultNamespace namespace : module.getNamespaces().values())
            visitNamespace(namespace);
        return null;
    }
}

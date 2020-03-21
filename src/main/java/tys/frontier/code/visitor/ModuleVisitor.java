package tys.frontier.code.visitor;

import tys.frontier.code.module.Module;

import java.util.List;

public interface ModuleVisitor<Mod, Namespace, Class, Field, Function, Statement, Expression> extends ClassVisitor<Namespace, Class, Field, Function, Statement, Expression> {

    //Top down
    default void enterModule(Module module) {}

    //Bottom Up
    default Mod exitModule(Module module, List<Namespace> namespaces) {
        return null;
    }
}

package tys.frontier.code.visitor;

import tys.frontier.code.module.Module;

import java.util.List;

public interface ModuleVisitor<Mod, Class, Field, Function, Statement, Expression> extends ClassVisitor<Class, Field, Function, Statement, Expression> {

    //Top down
    default void enterModule(Module module) {}

    //Bottom Up
    default Mod exitModule(Module module, List<Class> classes) {
        return null;
    }
}

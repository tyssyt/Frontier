package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.module.Module;
import tys.frontier.code.statement.FStatement;

import java.util.List;

public interface FModuleVisitor extends FClassVisitor, ModuleVisitor<Module, FClass, FField, FFunction, FStatement, FExpression> {
    @Override
    default Module exitModule(Module module, List<FClass> fClasses) {
        return module;
    }
}

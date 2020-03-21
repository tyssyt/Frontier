package tys.frontier.passes.lowering;

import tys.frontier.code.function.FFunction;
import tys.frontier.code.module.Module;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.FModuleVisitor;

import java.util.List;

public abstract class StatementReplacer implements FModuleVisitor {

    protected Module currentModule;
    protected DefaultNamespace currentNamespace;
    protected FFunction currentFunction;

    @Override
    public void enterModule(Module module) {
        currentModule = module;
    }

    @Override
    public void enterNamespace(DefaultNamespace namespace) {
        currentNamespace = namespace;
    }

    @Override
    public void enterFunction(FFunction function) {
        currentFunction = function;
    }

    @Override
    public FBlock exitBlock(FBlock block, List<FStatement> fStatements) {
        block.setStatements(fStatements);
        return block;
    }
}

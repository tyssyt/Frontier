package tys.frontier.passes.lowering;

import tys.frontier.code.FType;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.module.Module;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.FModuleVisitor;

import java.util.List;

public abstract class StatementReplacer implements FModuleVisitor {

    protected Module currentModule;
    protected FType currentType;
    protected FFunction currentFunction;

    @Override
    public void enterModule(Module module) {
        currentModule = module;
    }
    @Override
    public void enterType(FType clazz) {
        currentType = clazz;
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

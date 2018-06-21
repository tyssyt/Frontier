package tys.frontier.passes.lowering;

import tys.frontier.code.FClass;
import tys.frontier.code.FFile;
import tys.frontier.code.FFunction;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.FFileVisitor;

import java.util.List;

public abstract class StatementReplacer implements FFileVisitor {

    protected FFile currentFile;
    protected FClass currentClass;
    protected FFunction currentFunction;

    @Override
    public void enterFile(FFile file) {
        currentFile = file;
    }
    @Override
    public void enterClass(FClass clazz) {
        currentClass = clazz;
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
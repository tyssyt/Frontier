package tys.frontier.code.visitor;

import tys.frontier.code.FField;
import tys.frontier.code.FFile;
import tys.frontier.code.FFunction;
import tys.frontier.code.FType;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.statement.FStatement;

import java.util.List;

public interface FFileVisitor extends FClassVisitor, FileVisitor<FFile, FType, FField, FFunction, FStatement, FExpression> {
    @Override
    default FFile exitFile(FFile file, List<FType> fTypes) {
        return file;
    }
}

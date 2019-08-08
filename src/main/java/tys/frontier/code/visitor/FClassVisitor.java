package tys.frontier.code.visitor;

import tys.frontier.code.FField;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.type.FType;

import java.util.List;
import java.util.Optional;

public interface FClassVisitor extends FStatementVisitor, ClassVisitor<FType, FField, FFunction, FStatement, FExpression> {
    @Override
    default FType exitType(FType fClass, List<FField> fFields, List<FFunction> fFunctions) {
        return fClass;
    }

    @Override
    default FField exitField(FField field, Optional<FExpression> assign) {
        return field;
    }

    @Override
    default FFunction exitFunction(FFunction function, Optional<FStatement> body) {
        return function;
    }
}

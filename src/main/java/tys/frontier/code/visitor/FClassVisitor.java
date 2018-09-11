package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.statement.FStatement;

import java.util.List;
import java.util.Optional;

public interface FClassVisitor extends FStatementVisitor, ClassVisitor<FClass, FField, FFunction, FStatement, FExpression> {
    @Override
    default FClass exitType(FClass fClass, List<FField> fFields, List<FFunction> fFunctions) {
        return fClass;
    }

    @Override
    default FField exitField(FField field, Optional<FStatement> assign) {
        return field;
    }

    @Override
    default FFunction exitFunction(FFunction function, Optional<FStatement> body) {
        return function;
    }
}

package tys.frontier.code.visitor;

import tys.frontier.code.FField;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.type.FClass;

import java.util.List;
import java.util.Optional;

public interface FClassVisitor extends FStatementVisitor, ClassVisitor<DefaultNamespace, FClass, FField, FFunction, FStatement, FExpression> {

    @Override
    default DefaultNamespace exitNamespace(DefaultNamespace namespace, Optional<FClass> _class, List<FField> staticFields, List<FFunction> fFunctions) {
        return namespace;
    }

    @Override
    default FClass exitClass(FClass fClass, List<FField> instanceFields) {
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

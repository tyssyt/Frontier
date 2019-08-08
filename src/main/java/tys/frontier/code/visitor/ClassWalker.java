package tys.frontier.code.visitor;

import tys.frontier.code.FField;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.type.FClass;

public interface ClassWalker<Class, Field, Function, Statement, Expression> extends StatementWalker<Statement, Expression> {

    default Class visitClass(FClass fClass) {
        for (FField field : fClass.getFields())
            visitField(field);
        for (FFunction function : fClass.getFunctions().values())
            visitFunction(function);
        return null;
    }

    default Field visitField(FField field) {
        field.getAssignment().ifPresent(assignment -> assignment.accept(this));
        return null;
    }

    default Function visitFunction(FFunction function) {
        function.getBody().ifPresent(body -> body.accept(this));
        return null;
    }
}

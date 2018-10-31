package tys.frontier.code.visitor;

import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FType;

public interface ClassWalker<Class, Field, Function, Statement, Expression> extends StatementWalker<Statement, Expression> {

    default Class visitType(FType fClass) {
        for (FField field : fClass.getFields())
            visitField(field);
        for (FFunction function : fClass.getFunctions())
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

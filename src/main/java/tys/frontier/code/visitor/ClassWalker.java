package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.statement.FStatement;

public interface ClassWalker<Class, Field, Function, Statement, Expression> extends StatementWalker<Statement, Expression> {

    default Class visitClass(FClass clazz) {
        for (FField field : clazz.getFields().values())
            visitField(field);
        for (FFunction function : clazz.getFunctions().values())
            visitFunction(function);
        return null;
    }

    default Field visitField(FField field) {
        field.getAssignment().ifPresent(assignment -> assignment.accept(this));
        return null;
    }

    default Function visitFunction(FFunction function) {
        for (FStatement statement : function.getBody())
            statement.accept(this);
        return null;
    }
}

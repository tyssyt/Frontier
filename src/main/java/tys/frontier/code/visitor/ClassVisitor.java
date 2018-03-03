package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;

import java.util.List;
import java.util.Optional;

public interface ClassVisitor<Class, Field, Function, Statement, Expression> extends StatementVisitor<Statement, Expression> {

    //Top down
    default void enterClass(FClass clazz) {}
    default void enterField(FField field) {}
    default void enterFunction(FFunction function) {}

    //Bottom Up
    default Class exitClass(FClass clazz, List<Field> fields, List<Function> functions) {return null;}
    default Field exitField(FField field, Optional<Statement> assign) {return null;}
    default Function exitFunction(FFunction function, List<Statement> body) {return null;}
}

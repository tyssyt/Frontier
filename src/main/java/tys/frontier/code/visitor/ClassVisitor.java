package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;

import java.util.List;
import java.util.Optional;

//TODO this is still called a class visitor even though what we visit is a type, at some point that naming should be consistent again (same for subclasses of this)
public interface ClassVisitor<Class, Field, Function, Statement, Expression> extends StatementVisitor<Statement, Expression> {

    //Top down
    default void enterType(FClass fClass) {}
    default void enterField(FField field) {}
    default void enterFunction(FFunction function) {}

    //Bottom Up
    default Class exitType(FClass fClass, List<Field> fields, List<Function> functions) {return null;}
    default Field exitField(FField field, Optional<Statement> assign) {return null;}
    default Function exitFunction(FFunction function, Optional<Statement> body) {return null;}
}

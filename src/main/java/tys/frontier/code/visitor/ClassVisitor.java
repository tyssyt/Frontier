package tys.frontier.code.visitor;

import tys.frontier.code.FField;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FClass;

import java.util.List;
import java.util.Optional;

//TODO this is still called a class visitor even though what we visit is a type, at some point that naming should be consistent again (same for subclasses of this)
@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public interface ClassVisitor<Namespace, Class, Field, Function, Statement, Expression> extends StatementVisitor<Statement, Expression> {

    //Top down
    default void enterNamespace(DefaultNamespace namespace) {}
    default void enterClass(FClass fClass) {}
    default void enterField(FField field) {}
    default void enterFunction(FFunction function) {}

    //Bottom Up
    default Namespace exitNamespace(DefaultNamespace namespace, Optional<Class> _class, List<Function> functions) {return null;}
    default Class exitClass(FClass fClass, List<Field> fields) {return null;}
    default Field exitField(FField field, Optional<Expression> assign) {return null;}
    default Function exitFunction(FFunction function, Optional<Statement> body) {return null;}
}

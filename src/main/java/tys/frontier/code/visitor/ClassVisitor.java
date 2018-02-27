package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.statement.FStatement;

import java.util.List;
import java.util.Optional;

public interface ClassVisitor<Class, Field, Function, Statement> {

    default Class visit(FClass clazz) {
        return clazz.accept(this);
    }
    Statement visit(FStatement statement);

    //Top down
    default void enterClass(FClass clazz) {}
    default void enterField(FField field) {}
    default void enterFunction(FFunction function) {}

    //Bottom Up
    default Class exitClass(FClass clazz, List<Field> fields, List<Function> functions) {return null;}
    default Field exitField(FField field, Optional<Statement> assign) {return null;}
    default Function exitFunction(FFunction function, List<Statement> body) {return null;}

    abstract class Default<Class, Field, Function, Statement, Expression> implements ClassVisitor<Class, Field, Function, Statement> {
        protected StatementVisitor<Statement, Expression> stVis;

        public Default(StatementVisitor<Statement, Expression> statementVisitor) {
            this.stVis = statementVisitor;
        }

        @Override
        public Statement visit(FStatement statement) {
            return stVis.visit(statement);
        }
    }
}

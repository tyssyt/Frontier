package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.statement.FStatement;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public interface ClassVisitor<Class, Field, Function, Statement> {

    Class enterClass(FClass clazz);

    Field enterField(FField field);

    Function enterFunction(FFunction function);

    Class exitClass(FClass clazz, List<Field> fields, List<Function> functions);

    Field exitField(FField field, Optional<Statement> assign);

    Function exitFunction(FFunction function, List<Statement> body);

    class Default<Class, Field, Function, Statement, Expression> implements ClassVisitor<Class, Field, Function, Statement> {

        private StatementVisitor<Statement, Expression> stVis;

        public Default(StatementVisitor<Statement, Expression> statementVisitor) {
            this.stVis = statementVisitor;
        }

        @Override
        public Class enterClass(FClass clazz) {
            List<Field> fields = new ArrayList<>(clazz.getFields().size());
            for (FField f : clazz.getFields().values())
                fields.add(enterField(f));
            List<Function> functions = new ArrayList<>(clazz.getFunctions().size());
            for (FFunction f : clazz.getFunctions().values())
                functions.add(enterFunction(f));
            return exitClass(clazz, fields, functions);
        }

        @Override
        public Field enterField(FField field) {
            Optional<Statement> assign = field.getAssignment().map(a ->stVis.enterStatement((a)));
            return exitField(field, assign);
        }

        @Override
        public Function enterFunction(FFunction function) {
            List<Statement> body = new ArrayList<>(function.getBody().size());
            for (FStatement s : function.getBody())
                body.add(stVis.enterStatement(s));
            return exitFunction(function, body);
        }

        @Override
        public Class exitClass(FClass clazz, List<Field> fields, List<Function> functions) {
            return null;
        }

        @Override
        public Field exitField(FField field, Optional<Statement> assign) {
            return null;
        }

        @Override
        public Function exitFunction(FFunction function, List<Statement> body) {
            return null;
        }
    }
}

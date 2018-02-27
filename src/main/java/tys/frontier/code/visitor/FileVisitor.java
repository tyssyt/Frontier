package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FFile;

import java.util.List;

public interface FileVisitor<File, Class> {

    default File visit(FFile file) {
        return file.accept(this);
    }
    Class visit(FClass clazz);

    //Top down
    default void enterFile(FFile file) {}

    //Bottom Up
    default File exitFile(FFile file, List<Class> classes) {
        return null;
    }

    abstract class Default<File, Class, Field, Function, Statement> implements FileVisitor<File, Class> {
        private ClassVisitor<Class, Field, Function, Statement> classVisitor;

        public Default(ClassVisitor<Class, Field, Function, Statement> classVisitor) {
            this.classVisitor = classVisitor;
        }

        @Override
        public Class visit(FClass clazz) {
            return classVisitor.visit(clazz);
        }
    }

}

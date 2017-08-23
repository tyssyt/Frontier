package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FFile;

import java.util.ArrayList;
import java.util.List;

public interface FileVisitor<File, Class> {

    File enterFile(FFile file);

    File exitFile(FFile file, List<Class> classes);

    class Default<File, Class, Field, Function, Statement> implements FileVisitor<File, Class> {

        private ClassVisitor<Class, Field, Function, Statement> classVisitor;

        public Default(ClassVisitor<Class, Field, Function, Statement> classVisitor) {
            this.classVisitor = classVisitor;
        }

        @Override
        public File enterFile(FFile file) {
            List<Class> classes = new ArrayList<>(file.getClasses().size());
            for (FClass c : file.getClasses().values())
                classes.add(classVisitor.enterClass(c));
            return exitFile(file, classes);
        }

        @Override
        public File exitFile(FFile file, List<Class> classes) {
            return null;
        }
    }

}

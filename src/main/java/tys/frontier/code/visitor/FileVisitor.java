package tys.frontier.code.visitor;

import tys.frontier.code.FFile;

import java.util.List;

public interface FileVisitor<File, Class, Field, Function, Statement, Expression> extends ClassVisitor<Class, Field, Function, Statement, Expression> {

    //Top down
    default void enterFile(FFile file) {}

    //Bottom Up
    default File exitFile(FFile file, List<Class> classes) {
        return null;
    }
}

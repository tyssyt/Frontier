package tys.frontier.code.visitor;

import tys.frontier.code.FClass;
import tys.frontier.code.FFile;

public interface FileWalker<File, Class, Field, Function, Statement, Expression> extends ClassWalker<Class, Field, Function, Statement, Expression> {

    default File enterFile(FFile file) {
        for (FClass fClass : file.getTypes().values()) {
            visitType(fClass);
        }
        return null;
    }
}

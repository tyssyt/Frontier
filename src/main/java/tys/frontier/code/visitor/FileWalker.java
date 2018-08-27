package tys.frontier.code.visitor;

import tys.frontier.code.FFile;
import tys.frontier.code.FType;

public interface FileWalker<File, Class, Field, Function, Statement, Expression> extends ClassWalker<Class, Field, Function, Statement, Expression> {

    default File enterFile(FFile file) {
        for (FType fType : file.getTypes().values()) {
            visitType(fType);
        }
        return null;
    }
}

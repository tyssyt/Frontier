package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FType;

public class CyclicClassHierachy extends SyntaxError {

    public final FType cyclicClass;

    public CyclicClassHierachy(FType cyclicClass) {
        super(cyclicClass.getIdentifier() + " has a cyclic class hierachy");
        this.cyclicClass = cyclicClass;
    }
}

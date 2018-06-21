package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FClass;

public class CyclicClassHierachy extends SyntaxError {

    public final FClass cyclicClass;

    public CyclicClassHierachy(FClass cyclicClass) {
        super(cyclicClass.getIdentifier() + " has a cyclic class hierachy");
        this.cyclicClass = cyclicClass;
    }
}

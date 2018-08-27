package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FClass;

public class TwoSuperClasses extends SyntaxError {

    public final FClass subClass;
    public final FClass firstSuper;
    public final FClass secondSuper;

    public TwoSuperClasses(FClass subClass, FClass firstSuper, FClass secondSuper) {
        super(subClass.getIdentifier() + " has 2 super classes, " + firstSuper.getIdentifier() + " and " + secondSuper.getIdentifier());
        this.subClass = subClass;
        this.firstSuper = firstSuper;
        this.secondSuper = secondSuper;
    }
}

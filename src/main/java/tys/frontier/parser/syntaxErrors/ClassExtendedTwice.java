package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FClass;

public class ClassExtendedTwice extends SyntaxError {

    public FClass child;
    public FClass parent;

    public ClassExtendedTwice(FClass child, FClass parent) {
        super(child.getIdentifier() + " extends " + parent.getIdentifier() + " twice");
        this.child = child;
        this.parent = parent;
    }
}

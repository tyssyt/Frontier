package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FClass;

public class MultiExtend extends SyntaxError {

    public final FClass includer;
    public final FClass included;

    public MultiExtend(FClass includer, FClass included) {
        super(includer.getIdentifier() + " extends " + included.getIdentifier() + ", which has fields, multiple times");
        this.includer = includer;
        this.included = included;
    }
}

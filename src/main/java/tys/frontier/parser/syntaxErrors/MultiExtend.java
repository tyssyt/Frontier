package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FType;

public class MultiExtend extends SyntaxError {

    public final FType includer;
    public final FType included;

    public MultiExtend(FType includer, FType included) {
        super(includer.getIdentifier() + " extends " + included.getIdentifier() + ", which has fields, multiple times");
        this.includer = includer;
        this.included = included;
    }
}

package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FFunction;

public class OverridesWithLessVisibility extends SyntaxError {

    public final FFunction overwritten;
    public final FFunction overrides;

    public OverridesWithLessVisibility(FFunction overwritten, FFunction overrides) {
        super(overrides.getIdentifier() + ", which is " + overrides.getVisibility() + " overrides " +
              overwritten.getIdentifier() + ", which is " +overwritten.getVisibility());
        this.overwritten = overwritten;
        this.overrides = overrides;
    }
}

package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FFunction;

public class AccessForbidden extends SyntaxError {

    public final FFunction accessed;

    public AccessForbidden(FFunction accessed) {
        super("tried to access: " + accessed.getIdentifier() +", but it is private");
        assert accessed.getVisibility() == FVisibilityModifier.PRIVATE;
        this.accessed = accessed;
    }
}

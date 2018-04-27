package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.HasVisibility;

public class AccessForbidden extends SyntaxError {

    public final HasVisibility accessed;

    public AccessForbidden(HasVisibility accessed) {
        super("tried to access: " + accessed +", but it is private");
        assert accessed.getVisibility() == FVisibilityModifier.PRIVATE;
        this.accessed = accessed;
    }
}

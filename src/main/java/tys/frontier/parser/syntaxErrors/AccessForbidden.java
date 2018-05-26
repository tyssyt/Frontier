package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.HasVisibility;
import tys.frontier.code.identifier.IdentifierNameable;

public class AccessForbidden extends SyntaxError {

    public final HasVisibility accessed;

    public AccessForbidden(HasVisibility accessed) {
        super("tried to access: " + (accessed instanceof IdentifierNameable ? ((IdentifierNameable) accessed).getIdentifier() : accessed) +", but it is private");
        assert accessed.getVisibility() == FVisibilityModifier.PRIVATE;
        this.accessed = accessed;
    }
}

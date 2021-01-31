package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FFunction;
import tys.frontier.parser.location.Position;

public class AccessForbidden extends SyntaxError {

    public final FFunction accessed;

    public AccessForbidden(Position position, FFunction accessed) {
        super(position, "tried to access: " + accessed.getIdentifier() +", but it is private");
        assert accessed.getVisibility() == FVisibilityModifier.PRIVATE;
        this.accessed = accessed;
    }
}

package tys.frontier.code.identifier;

public class FErrorIdentifier extends FClassIdentifier {

    public final FClassIdentifier identifier;

    public FErrorIdentifier(FClassIdentifier identifier) {
        super("ERROR(" + identifier + ')');
        this.identifier = identifier;
    }
}

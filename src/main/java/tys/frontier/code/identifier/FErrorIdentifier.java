package tys.frontier.code.identifier;

public class FErrorIdentifier extends FTypeIdentifier {

    public final FTypeIdentifier identifier;

    public FErrorIdentifier(FTypeIdentifier identifier) {
        super("ERROR(" + identifier + ')');
        this.identifier = identifier;
    }
}

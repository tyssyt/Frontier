package tys.frontier.code.identifier;

public class FClassIdentifier extends FIdentifier {

    public FClassIdentifier(String name) {
        super(name);
        assert (name.startsWith(name.substring(0,1).toUpperCase()));
    }
}

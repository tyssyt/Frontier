package tys.frontier.code.identifier;

public class FFunctionIdentifier extends FIdentifier {

    public FFunctionIdentifier(String name) {
        super(name);
        assert (name.startsWith(name.substring(0,1).toLowerCase()));
    }
}

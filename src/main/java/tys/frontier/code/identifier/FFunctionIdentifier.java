package tys.frontier.code.identifier;

public class FFunctionIdentifier extends FIdentifier {

    public FFunctionIdentifier(String name) {
        super(name);
        assert (this.name.startsWith(this.name.substring(0,1).toLowerCase()));
    }
}

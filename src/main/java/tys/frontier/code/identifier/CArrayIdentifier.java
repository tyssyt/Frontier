package tys.frontier.code.identifier;

public class CArrayIdentifier extends FIdentifier {

    public CArrayIdentifier(FIdentifier base) {
        super("native[" + base.name + ']');
    }
}

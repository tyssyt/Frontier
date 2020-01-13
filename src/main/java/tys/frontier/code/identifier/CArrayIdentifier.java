package tys.frontier.code.identifier;

public class CArrayIdentifier extends FIdentifier {

    public final FIdentifier base;

    public CArrayIdentifier(FIdentifier base) {
        super(base.name + "[c]");
        this.base = base;
    }
}

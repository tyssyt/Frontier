package tys.frontier.code.identifier;

public class CArrayIdentifier extends FTypeIdentifier {

    public final FTypeIdentifier base;

    public CArrayIdentifier(FTypeIdentifier base) {
        super(base.name + "[c]");
        this.base = base;
    }
}

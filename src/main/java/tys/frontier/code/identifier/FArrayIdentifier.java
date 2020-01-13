package tys.frontier.code.identifier;

public class FArrayIdentifier extends FIdentifier {

    public final FIdentifier baseClass;

    public FArrayIdentifier(FIdentifier baseClass) {
        super(baseClass.name + "[]");
        this.baseClass = baseClass;
    }
}

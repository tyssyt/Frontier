package tys.frontier.code.identifier;

public class FArrayIdentifier extends FClassIdentifier {

    public final FClassIdentifier baseClass;

    public FArrayIdentifier(FClassIdentifier baseClass) {
        super(baseClass.name + "[]");
        this.baseClass = baseClass;
    }
}

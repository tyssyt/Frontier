package tys.frontier.code.identifier;

public class FArrayIdentifier extends FTypeIdentifier {

    public final FTypeIdentifier baseClass;

    public FArrayIdentifier(FTypeIdentifier baseClass) {
        super(baseClass.name + "[]");
        this.baseClass = baseClass;
    }
}

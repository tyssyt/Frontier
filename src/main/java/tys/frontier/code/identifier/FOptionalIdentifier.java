package tys.frontier.code.identifier;

public class FOptionalIdentifier extends FTypeIdentifier {

    public final FTypeIdentifier baseClass;

    public FOptionalIdentifier(FTypeIdentifier baseClass) {
        super(baseClass.name + "?");
        this.baseClass = baseClass;
    }
}

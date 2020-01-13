package tys.frontier.code.identifier;

public class FOptionalIdentifier extends FIdentifier {

    public final FIdentifier baseClass;

    public FOptionalIdentifier(FIdentifier baseClass) {
        super(baseClass.name + "?");
        this.baseClass = baseClass;
    }
}

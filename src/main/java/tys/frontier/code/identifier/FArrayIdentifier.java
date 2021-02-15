package tys.frontier.code.identifier;

public class FArrayIdentifier extends FIdentifier {

    public FArrayIdentifier(FIdentifier baseClass) {
        super('[' + baseClass.name + ']');
    }
}

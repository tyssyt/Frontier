package tys.frontier.code.identifier;

public class FIntIdentifier extends FIdentifier {

    public static final String INT_ID = "!Int";
    public final int n;

    public FIntIdentifier(int n) {
        super(INT_ID + n);
        this.n = n;
    }
}
package tys.frontier.code.identifier;

public class FTypeIdentifier extends FIdentifier {

    public static final FTypeIdentifier BOOL = new FTypeIdentifier("!Bool");
    public static final FTypeIdentifier FLOAT32 = new FTypeIdentifier("!Float32");
    public static final FTypeIdentifier FLOAT64 = new FTypeIdentifier("!Float64");
    public static final FTypeIdentifier VOID = new FTypeIdentifier("!Void");

    public FTypeIdentifier(String name) {
        super(name);
        assert (this.name.startsWith(this.name.substring(0,1).toUpperCase()));
    }
}

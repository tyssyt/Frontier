package tys.frontier.code.identifier;

public class FClassIdentifier extends FIdentifier {

    public static final FClassIdentifier BOOL = new FClassIdentifier("!Bool");
    public static final FClassIdentifier INT = new FClassIdentifier("!Int");
    public static final FClassIdentifier INT32 = new FClassIdentifier("!Int32");
    public static final FClassIdentifier INT64 = new FClassIdentifier("!Int64");
    public static final FClassIdentifier FLOAT32 = new FClassIdentifier("!Float32");
    public static final FClassIdentifier FLOAT64 = new FClassIdentifier("!Float64");
    public static final FClassIdentifier VOID = new FClassIdentifier("!Void");

    public FClassIdentifier(String name) {
        super(name);
        assert (name.startsWith(name.substring(0,1).toUpperCase()));
    }
}

package tys.frontier.code.type;

public class FVoid implements FType {

    public static final FVoid INSTANCE = new FVoid();

    private FVoid () {}

    @Override
    public String toString() {
        return "void";
    }
}

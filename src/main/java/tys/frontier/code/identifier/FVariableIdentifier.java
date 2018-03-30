package tys.frontier.code.identifier;

public class FVariableIdentifier extends FIdentifier {

    public static final FVariableIdentifier THIS = new FVariableIdentifier("!this"); //use a name that can't be an identifier in the parser
    public static final FVariableIdentifier SIZE = new FVariableIdentifier("size");

    public FVariableIdentifier(String name) {
        super(name);
        assert (name.startsWith(name.substring(0,1).toLowerCase()));
    }
}

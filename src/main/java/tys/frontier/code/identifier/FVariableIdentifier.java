package tys.frontier.code.identifier;

public class FVariableIdentifier extends FIdentifier {

    public static final FVariableIdentifier THIS = new FVariableIdentifier("!this"); //use a name that can't be an identifier in the parser

    public FVariableIdentifier(String name) {
        super(name);
        assert (this.name.startsWith(this.name.substring(0,1).toLowerCase()));
    }
}

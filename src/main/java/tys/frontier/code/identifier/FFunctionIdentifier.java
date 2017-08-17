package tys.frontier.code.identifier;

public class FFunctionIdentifier extends FIdentifier {

    public static final FFunctionIdentifier CONSTRUCTOR = new FFunctionIdentifier("!new"); //take a name that would be invalid for the parser to avoid clashing with user defined functions

    public FFunctionIdentifier(String name) {
        super(name);
        assert (name.startsWith(name.substring(0,1).toLowerCase()));
    }
}

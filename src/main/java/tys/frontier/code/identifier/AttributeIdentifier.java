package tys.frontier.code.identifier;

public class AttributeIdentifier extends FIdentifier {

    public static final AttributeIdentifier THIS = new AttributeIdentifier("!this"); //use a name that can't be an identifier in the parser

    public AttributeIdentifier(String name) {
        super(name);
        assert (this.name.startsWith(this.name.substring(0,1).toLowerCase()));
    }
}

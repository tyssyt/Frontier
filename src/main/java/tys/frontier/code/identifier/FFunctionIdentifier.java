package tys.frontier.code.identifier;

public class FFunctionIdentifier extends FIdentifier {

    public static final FFunctionIdentifier CONSTRUCTOR = new FFunctionIdentifier("!new"); //take a name that would be invalid for the parser to avoid clashing with user defined functions

    public static final FFunctionIdentifier EQUALS = new FFunctionIdentifier("==");
    public static final FFunctionIdentifier NOT_EQUALS = new FFunctionIdentifier("!=");
    public static final FFunctionIdentifier HASHCODE = new FFunctionIdentifier("#");

    public static final FFunctionIdentifier NOT = new FFunctionIdentifier("!");
    public static final FFunctionIdentifier AND = new FFunctionIdentifier("&&");
    public static final FFunctionIdentifier OR = new FFunctionIdentifier("||");
    public static final FFunctionIdentifier XOR = new FFunctionIdentifier("^");

    public static final FFunctionIdentifier PLUS = new FFunctionIdentifier("+");
    public static final FFunctionIdentifier MINUS = new FFunctionIdentifier("-");
    public static final FFunctionIdentifier TIMES = new FFunctionIdentifier("*");
    public static final FFunctionIdentifier DIVIDED = new FFunctionIdentifier("/");
    public static final FFunctionIdentifier MODULO = new FFunctionIdentifier("%");

    public static final FFunctionIdentifier SMALLER = new FFunctionIdentifier("<");
    public static final FFunctionIdentifier GREATER = new FFunctionIdentifier(">");
    public static final FFunctionIdentifier SMALLER_EQUAL = new FFunctionIdentifier("<=");
    public static final FFunctionIdentifier GREATER_EQUAL = new FFunctionIdentifier(">=");

    public static final FFunctionIdentifier ARRAY_ACCESS = new FFunctionIdentifier("[]");
    public static final FFunctionIdentifier ARRAY_RANGE_ACCESS = new FFunctionIdentifier("[-]");

    public FFunctionIdentifier(String name) {
        super(name);
        assert (name.startsWith(name.substring(0,1).toLowerCase()));
    }
}

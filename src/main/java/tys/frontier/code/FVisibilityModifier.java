package tys.frontier.code;

public enum FVisibilityModifier {
    PUBLIC ("public"),
    PRIVATE ("private"),
    NONE ("package-private");

    public final String text;

    FVisibilityModifier (String s) {
        text = s;
    }

    @Override
    public String toString() {
        return text;
    }
}

package tys.frontier.code;

import tys.frontier.parser.antlr.FrontierParser;

public enum FVisibilityModifier {
    PRIVATE ("private", FrontierParser.PRIVATE),
    NONE("", 0),
    EXPORT ("export", FrontierParser.EXPORT);

    public final String text;
    public final int tokenId;

    FVisibilityModifier (String s, int tokenId) {
        text = s;
        this.tokenId = tokenId;
    }

    @Override
    public String toString() {
        return text;
    }
}

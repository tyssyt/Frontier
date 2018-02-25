package tys.frontier.code;

import tys.frontier.parser.antlr.FrontierParser;

public enum FVisibilityModifier {
    PUBLIC ("public", FrontierParser.PUBLIC),
    PRIVATE ("private", FrontierParser.PRIVATE);

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

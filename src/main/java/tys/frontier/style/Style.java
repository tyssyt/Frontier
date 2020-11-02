package tys.frontier.style;

import com.google.common.collect.BiMap;

public class Style {

    public static final Style DEFAULT_STYLE = new Style(Keywords.DEFAULT_KEYWORDS, "default");

    private BiMap<String, Integer> keywords;
    private String name;

    private Style(BiMap<String, Integer> keywords, String name) {
        this.keywords = keywords;
        this.name = name;
    }

    public BiMap<String, Integer> getKeywords() {
        return keywords;
    }

    public String getKeyword(int tokenId) {
        return keywords.inverse().get(tokenId);
    }

    @Override
    public String toString() {
        return "Style(" + name + ")";
    }
}

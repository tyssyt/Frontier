package tys.frontier.style;

import com.google.common.collect.BiMap;
import com.opensymphony.xwork2.util.ClassLoaderUtil;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.IOException;
import java.net.URL;

public class Style {

    private static final String DEFAULT_LOCATION = "frontier.style"; //TODO once we got things further this location should change

    public static final Style DEFAULT_STYLE = new Style(Keywords.DEFAULT_KEYWORDS, StyleOptions.DEFAULT_STYLE_OPTIONS, "default");
    public static final Style NAME_PENDING = new Style(Keywords.DEFAULT_KEYWORDS, StyleOptions.NAME_PENDING, "pending");
    public static final Style USER_STYLE = fromFile(DEFAULT_LOCATION, true);

    private BiMap<String, Integer> keywords;
    private StyleOptions options;
    private String name;

    //TODO do we really want to not throw if the file doesn't exist?
    public static Style fromFile(String location, boolean logWarning) {
        JSONObject styleFile;
        String name;
        try {
            URL url = ClassLoaderUtil.getResource(location, Style.class);
            styleFile = new JSONObject(new JSONTokener(url.openStream()));
            name = url.toString();
        } catch (JSONException | IOException e) {
            if (logWarning);
                //Log.warning(Style.class,  e);
            return DEFAULT_STYLE;
        }

        BiMap<String, Integer> keywords;
        try {
            keywords = Keywords.fromStyleFile(styleFile.getJSONObject("keywords"));
        } catch (JSONException e) {
            if (logWarning);
                //Log.warning(Style.class,  e);
            keywords = Keywords.DEFAULT_KEYWORDS;
        }

        StyleOptions options;
        try {
            options = StyleOptions.fromStyleFile(styleFile.getJSONObject("options"));
        } catch (JSONException e) {
            if (logWarning);
                //Log.warning(Style.class,  e);
            options = StyleOptions.DEFAULT_STYLE_OPTIONS;
        }

        return new Style(keywords, options, name);
    }

    private Style(BiMap<String, Integer> keywords, StyleOptions options, String name) {
        this.keywords = keywords;
        this.options = options;
        this.name = name;
    }

    public BiMap<String, Integer> getKeywords() {
        return keywords;
    }

    public StyleOptions getOptions() {
        return options;
    }

    public String getKeyword(int tokenId) {
        return keywords.inverse().get(tokenId);
    }

    public Indenter createIndenter() {
        return new Indenter(options);
    }

    @Override
    public String toString() {
        return "Style(" + name + ")";
    }
}

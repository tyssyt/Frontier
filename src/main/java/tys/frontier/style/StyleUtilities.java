package tys.frontier.style;

import com.google.common.collect.BiMap;
import com.opensymphony.xwork2.util.ClassLoaderUtil;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;
import tys.frontier.logging.Log;
import tys.frontier.util.Pair;

import java.io.*;

public class StyleUtilities {

    public static String DEFAULT_LOCATION = "frontier.style";

    public static Pair<StyleOptions, BiMap<String, Integer>> getStyleFileFrom (String location) throws FileNotFoundException, JSONException {
        Reader reader = new BufferedReader(new FileReader(location));
        JSONObject styleFile = new JSONObject(new JSONTokener(reader));
        return new Pair<>(StyleOptions.fromStyleFile(styleFile), Keywords.fromStyleFile(styleFile));
    }

    public static Pair<StyleOptions, BiMap<String, Integer>> getStyleFileFromOrDefault (String location, boolean logWarning) {
        JSONObject styleFile;
        try {
            InputStream input = ClassLoaderUtil.getResourceAsStream(location, StyleUtilities.class);
            styleFile = new JSONObject(new JSONTokener(input));
        } catch (JSONException e) {
            if (logWarning)
                Log.warning(StyleUtilities.class,  e);
            return new Pair<>(StyleOptions.DEFAULT_STYLE_OPTIONS, Keywords.DEFAULT_KEYWORDS);
        }

        StyleOptions options;
        try {
            options = StyleOptions.fromStyleFile(styleFile);
        } catch (JSONException e) {
            if (logWarning)
                Log.warning(StyleUtilities.class,  e);
            options = StyleOptions.DEFAULT_STYLE_OPTIONS;
        }

        BiMap<String, Integer> keywords;
        try {
            keywords = Keywords.fromStyleFile(styleFile);
        } catch (JSONException e) {
            if (logWarning)
                Log.warning(StyleUtilities.class,  e);
            keywords = Keywords.DEFAULT_KEYWORDS;
        }

        return new Pair<>(options, keywords);
    }
}
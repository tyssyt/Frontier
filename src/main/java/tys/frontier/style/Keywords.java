package tys.frontier.style;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableBiMap;
import org.json.JSONException;
import org.json.JSONObject;
import tys.frontier.parser.antlr.FrontierParser;

import java.util.Map;

public class Keywords {

    public static final ImmutableBiMap<String, Integer> DEFAULT_KEYWORDS = new ImmutableBiMap.Builder<String, Integer>()
            .put("import", FrontierParser.IMPORT)
            .put("class", FrontierParser.CLASS)
            .put("export", FrontierParser.EXPORT)
            .put("private", FrontierParser.PRIVATE)
            .put("static", FrontierParser.STATIC)
            .put("new", FrontierParser.NEW)
            .put("this", FrontierParser.THIS)
            .put("null", FrontierParser.NULL)
            .put("void", FrontierParser.VOID)
            .put("bool", FrontierParser.BOOL)
            .put("int", FrontierParser.INT)
            .put("int32", FrontierParser.INT32)
            .put("int64", FrontierParser.INT64)
            .put("float32", FrontierParser.FLOAT32)
            .put("float64", FrontierParser.FLOAT64)
            .put("if", FrontierParser.IF)
            .put("then", FrontierParser.THEN)
            .put("else", FrontierParser.ELSE)
            .put("while", FrontierParser.WHILE)
            .put("for", FrontierParser.FOR)
            .put("switch", FrontierParser.SWITCH)
            .put("case", FrontierParser.CASE)
            .put("default", FrontierParser.DEFAULT)
            .put("fallthrough", FrontierParser.FALLTHROUGH)
            .put("continue", FrontierParser.CONTINUE)
            .put("break", FrontierParser.BREAK)
            .put("return", FrontierParser.RETURN)
            .put("true", FrontierParser.TRUE)
            .put("false", FrontierParser.FALSE)
            .build();

    private Keywords(){}

    public static BiMap<String, Integer> fromStyleFile (JSONObject keywords) {
        BiMap<String, Integer> res = HashBiMap.create(DEFAULT_KEYWORDS.size());
        for (Map.Entry<String, Integer> keywordAndToken : DEFAULT_KEYWORDS.entrySet()) {
            String newKeyword;
            try {
                //see if the style redefines the keyword
                newKeyword = keywords.getString(keywordAndToken.getKey());
            } catch (JSONException e) {
                newKeyword = keywordAndToken.getKey();
            }
            res.put(newKeyword, keywordAndToken.getValue());
        }
        return res;
    }
}

package tys.frontier.style;

import com.google.common.collect.ImmutableBiMap;
import tys.frontier.parser.antlr.FrontierParser;

public class Keywords {

    public static final ImmutableBiMap<String, Integer> DEFAULT_KEYWORDS = new ImmutableBiMap.Builder<String, Integer>()
            .put("import", FrontierParser.IMPORT)
            .put("include", FrontierParser.INCLUDE)
            .put("class", FrontierParser.CLASS)
            .put("namespace", FrontierParser.NAMESPACE)
            .put("constructors", FrontierParser.CONSTRUCTORS)
            .put("export", FrontierParser.EXPORT)
            .put("private", FrontierParser.PRIVATE)
            .put("static", FrontierParser.STATIC)
            .put("native", FrontierParser.NATIVE)
            .put("open", FrontierParser.OPEN)
            .put("operator", FrontierParser.OPERATOR)
            .put("delegate", FrontierParser.DELEGATE)
            .put("in", FrontierParser.IN)
            .put("out", FrontierParser.OUT)
            .put("where", FrontierParser.WHERE)
            .put("this", FrontierParser.THIS)
            .put("null", FrontierParser.NULL)
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
            .put("bool", FrontierParser.BOOL)
            .put("int", FrontierParser.INT)
            .put("char", FrontierParser.CHAR)
            .put("int16", FrontierParser.INT16)
            .put("int32", FrontierParser.INT32)
            .put("int64", FrontierParser.INT64)
            .put("float32", FrontierParser.FLOAT32)
            .put("float64", FrontierParser.FLOAT64)
            .put("true", FrontierParser.TRUE)
            .put("false", FrontierParser.FALSE)
            .build();

    private Keywords(){}
}

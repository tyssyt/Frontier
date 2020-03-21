package tys.frontier.style;

import org.json.JSONException;
import org.json.JSONObject;

public class StyleOptions {

    public static final StyleOptions DEFAULT_STYLE_OPTIONS = builder().createStyleOptions();
    public static final StyleOptions NAME_PENDING = builder()
            .setUseTabs(false)
            .setIndention(0)
            .setMaxCharsPerLine(99999)
            .setMaxParamsPerLine(1)
            .setParamsOnNewLine(true)
            .setNoBracketsForSingleStatementConditional(false)
            .setNoBracketsForSingleStatementLoop(false)
            .setSpaceAfterFunctionName(false)
            .setSpaceAfterConditional(false)
            .setSpaceAfterLoop(false)
            .setSpaceAfterMethodCall(false)
            .setBracketsOnNewLineAfterClass(true)
            .setBracketsOnNewLineAfterFunction(true)
            .setBracketsOnNewLineAfterConditional(true)
            .setBracketsOnNewLineAfterLoop(true)
            .createStyleOptions();


    public final boolean useTabs;
    public final int indention;
    public final int maxCharsPerLine;
    public final int maxParamsPerLine;
    public final boolean paramsOnNewLine;

    public final boolean noBracketsForSingleStatementConditional;
    public final boolean noBracketsForSingleStatementLoop;

    public final boolean spaceAfterFunctionName;
    public final boolean spaceAfterConditional;
    public final boolean spaceAfterLoop;
    public final boolean spaceAfterMethodCall;

    public final boolean bracketsOnNewLineAfterClass;
    public final boolean bracketsOnNewLineAfterFunction;
    public final boolean bracketsOnNewLineAfterConditional;
    public final boolean bracketsOnNewLineAfterLoop;

    StyleOptions(boolean useTabs, int indention, int maxCharsPerLine, int maxParamsPerLine, boolean paramsOnNewLine, boolean noBracketsForSingleStatementConditional, boolean noBracketsForSingleStatementLoop, boolean spaceAfterFunctionName, boolean spaceAfterConditional, boolean spaceAfterLoop, boolean spaceAfterMethodCall, boolean bracketsOnNewLineAfterClass, boolean bracketsOnNewLineAfterFunction, boolean bracketsOnNewLineAfterConditional, boolean bracketsOnNewLineAfterLoop) {
        this.useTabs = useTabs;
        this.indention = indention;
        this.maxCharsPerLine = maxCharsPerLine;
        this.maxParamsPerLine = maxParamsPerLine;
        this.paramsOnNewLine = paramsOnNewLine;
        this.noBracketsForSingleStatementConditional = noBracketsForSingleStatementConditional;
        this.noBracketsForSingleStatementLoop = noBracketsForSingleStatementLoop;
        this.spaceAfterFunctionName = spaceAfterFunctionName;
        this.spaceAfterConditional = spaceAfterConditional;
        this.spaceAfterLoop = spaceAfterLoop;
        this.spaceAfterMethodCall = spaceAfterMethodCall;
        this.bracketsOnNewLineAfterClass = bracketsOnNewLineAfterClass;
        this.bracketsOnNewLineAfterFunction = bracketsOnNewLineAfterFunction;
        this.bracketsOnNewLineAfterConditional = bracketsOnNewLineAfterConditional;
        this.bracketsOnNewLineAfterLoop = bracketsOnNewLineAfterLoop;
    }

    public static StyleOptionsBuilder builder () {
        return new StyleOptionsBuilder();
    }

    //TODO there must be a better way to do this
    public static StyleOptions fromStyleFile (JSONObject options) {
        StyleOptionsBuilder builder = builder();

        try {
            builder.setUseTabs(options.getBoolean("useTabs"));
        } catch (JSONException ignored) {}
        try {
            builder.setIndention(options.getInt("indention"));
        } catch (JSONException ignored) {}
        try {
            builder.setMaxCharsPerLine(options.getInt("maxCharsPerLine"));
        } catch (JSONException ignored) {}
        try {
            builder.setMaxParamsPerLine(options.getInt("maxParamsPerLine"));
        } catch (JSONException ignored) {}
        try {
            builder.setParamsOnNewLine(options.getBoolean("paramsOnNewLine"));
        } catch (JSONException ignored) {}

        try {
            JSONObject noBracketsForSingleStatement = options.getJSONObject("noBracketsForSingleStatementAfter");
            try {
                builder.setNoBracketsForSingleStatementConditional(noBracketsForSingleStatement.getBoolean("conditional"));
            } catch (JSONException ignored) {}
            try {
                builder.setNoBracketsForSingleStatementLoop(noBracketsForSingleStatement.getBoolean("loop"));
            } catch (JSONException ignored) {}
        } catch (JSONException ignored) {}


        try {
            JSONObject spaceAfter = options.getJSONObject("spaceAfter");
            try {
                builder.setSpaceAfterFunctionName(spaceAfter.getBoolean("functionName"));
            } catch (JSONException ignored) {}
            try {
                builder.setSpaceAfterConditional(spaceAfter.getBoolean("conditional"));
            } catch (JSONException ignored) {}
            try {
                builder.setSpaceAfterLoop(spaceAfter.getBoolean("loop"));
            } catch (JSONException ignored) {}
            try {
                builder.setSpaceAfterMethodCall(spaceAfter.getBoolean("methodCall"));
            } catch (JSONException ignored) {}
        } catch (JSONException ignored) {}


        try {
            JSONObject bracketsOnNewLine = options.getJSONObject("bracketsOnNewLineAfter");
            try {
                builder.setBracketsOnNewLineAfterClass(bracketsOnNewLine.getBoolean("class"));
            } catch (JSONException ignored) {}
            try {
                builder.setBracketsOnNewLineAfterFunction(bracketsOnNewLine.getBoolean("function"));
            } catch (JSONException ignored) {}
            try {
                builder.setBracketsOnNewLineAfterConditional(bracketsOnNewLine.getBoolean("conditional"));
            } catch (JSONException ignored) {}
            try {
                builder.setBracketsOnNewLineAfterLoop(bracketsOnNewLine.getBoolean("loop"));
            } catch (JSONException ignored) {}
        } catch (JSONException ignored) {}

        return builder.createStyleOptions();
    }
}

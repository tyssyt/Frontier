package tys.frontier.style;

public class StyleOptionsBuilder {
    private boolean useTabs = false;
    private int indention = 4;
    private int maxCharsPerLine = 100;
    private int maxParamsPerLine = 9999;
    private boolean paramsOnNewLine = false;
    private boolean noBracketsForSingleStatementConditional = true;
    private boolean noBracketsForSingleStatementLoop = true;
    private boolean spaceAfterFunctionName = true;
    private boolean spaceAfterConditional = true;
    private boolean spaceAfterLoop = true;
    private boolean spaceAfterMethodCall = false;
    private boolean bracketsOnNewLineAfterClass = false;
    private boolean bracketsOnNewLineAfterFunction = false;
    private boolean bracketsOnNewLineAfterConditional = false;
    private boolean bracketsOnNewLineAfterLoop = false;

    public StyleOptionsBuilder setUseTabs(boolean useTabs) {
        this.useTabs = useTabs;
        return this;
    }

    public StyleOptionsBuilder setIndention(int indention) {
        if (indention > 0)
            this.indention = indention;
        return this;
    }

    public StyleOptionsBuilder setMaxCharsPerLine(int maxCharsPerLine) {
        if (maxCharsPerLine > 0)
            this.maxCharsPerLine = maxCharsPerLine;
        return this;
    }

    public StyleOptionsBuilder setMaxParamsPerLine(int maxParamsPerLine) {
        if (maxParamsPerLine > 0)
            this.maxParamsPerLine = maxParamsPerLine;
        return this;
    }

    public StyleOptionsBuilder setParamsOnNewLine(boolean paramsOnNewLine) {
        this.paramsOnNewLine = paramsOnNewLine;
        return this;
    }

    public StyleOptionsBuilder setNoBracketsForSingleStatementConditional(boolean noBracketsForSingleStatementIf) {
        this.noBracketsForSingleStatementConditional = noBracketsForSingleStatementIf;
        return this;
    }

    public StyleOptionsBuilder setNoBracketsForSingleStatementLoop(boolean noBracketsForSingleStatementLoop) {
        this.noBracketsForSingleStatementLoop = noBracketsForSingleStatementLoop;
        return this;
    }

    public StyleOptionsBuilder setSpaceAfterFunctionName(boolean spaceAfterFunctionName) {
        this.spaceAfterFunctionName = spaceAfterFunctionName;
        return this;
    }

    public StyleOptionsBuilder setSpaceAfterConditional(boolean spaceAfterConditional) {
        this.spaceAfterConditional = spaceAfterConditional;
        return this;
    }

    public StyleOptionsBuilder setSpaceAfterLoop(boolean spaceAfterLoop) {
        this.spaceAfterLoop = spaceAfterLoop;
        return this;
    }

    public StyleOptionsBuilder setSpaceAfterMethodCall(boolean spaceAfterMethodCall) {
        this.spaceAfterMethodCall = spaceAfterMethodCall;
        return this;
    }

    public StyleOptionsBuilder setBracketsOnNewLineAfterClass(boolean bracketsOnNewLineAfterClass) {
        this.bracketsOnNewLineAfterClass = bracketsOnNewLineAfterClass;
        return this;
    }

    public StyleOptionsBuilder setBracketsOnNewLineAfterFunction(boolean bracketsOnNewLineAfterFunction) {
        this.bracketsOnNewLineAfterFunction = bracketsOnNewLineAfterFunction;
        return this;
    }

    public StyleOptionsBuilder setBracketsOnNewLineAfterConditional(boolean bracketsOnNewLineAfterConditional) {
        this.bracketsOnNewLineAfterConditional = bracketsOnNewLineAfterConditional;
        return this;
    }

    public StyleOptionsBuilder setBracketsOnNewLineAfterLoop(boolean bracketsOnNewLineAfterLoop) {
        this.bracketsOnNewLineAfterLoop = bracketsOnNewLineAfterLoop;
        return this;
    }

    public StyleOptions createStyleOptions() {
        return new StyleOptions(useTabs, indention, maxCharsPerLine, maxParamsPerLine, paramsOnNewLine, noBracketsForSingleStatementConditional, noBracketsForSingleStatementLoop, spaceAfterFunctionName, spaceAfterConditional, spaceAfterLoop, spaceAfterMethodCall, bracketsOnNewLineAfterClass, bracketsOnNewLineAfterFunction, bracketsOnNewLineAfterConditional, bracketsOnNewLineAfterLoop);
    }
}
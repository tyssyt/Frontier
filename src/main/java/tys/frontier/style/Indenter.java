package tys.frontier.style;

public class Indenter {
    private int indentionLevel;
    private StringBuilder indentionString = new StringBuilder();

    private final int indentionDepth;
    private final String indentionIncrement;

    public Indenter(StyleOptions options) {
        indentionDepth = options.indention;
        char indentionChar = options.useTabs ? '\t' : ' ';
        StringBuilder sb = new StringBuilder();
        for (int i=0; i < indentionDepth; i++)
            sb.append(indentionChar);
        indentionIncrement = sb.toString();
    }

    public String newLine() {
        return '\n' + indentionString.toString();
    }
    public StringBuilder appendNewLine(StringBuilder sb) {
        return sb.append('\n').append(indentionString);
    }

    public void increase() {
        indentionLevel++;
        indentionString.append(indentionIncrement);
    }

    public void decrease() {
        indentionLevel--;
        indentionString.delete(indentionString.length()-indentionDepth, indentionString.length());
    }
}

package tys.frontier.parser.location;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;

public class Position {

    private int lineFrom;
    private int lineTo;
    private int columnFrom;
    private int columnTo;

    public Position(int lineFrom, int lineTo, int columnFrom, int columnTo) {
        this.lineFrom = lineFrom;
        this.lineTo = lineTo;
        this.columnFrom = columnFrom;
        this.columnTo = columnTo;
    }

    public static Position fromCtx(ParserRuleContext ctx) {
        Token start = ctx.getStart();
        Token stop = ctx.getStop();
        return new Position(start.getLine(), stop.getLine(), start.getCharPositionInLine(), stop.getCharPositionInLine() + stop.getText().length());
    }

    public static Position fromToken(Token token) {
        return new Position(token.getLine(), token.getLine(), token.getCharPositionInLine(), token.getCharPositionInLine() + token.getText().length());
    }

    public int getLineFrom() {
        return lineFrom;
    }

    public int getLineTo() {
        return lineTo;
    }

    public int getColumnFrom() {
        return columnFrom;
    }

    public int getColumnTo() {
        return columnTo;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Position position = (Position) o;

        if (lineFrom != position.lineFrom) return false;
        if (lineTo != position.lineTo) return false;
        if (columnFrom != position.columnFrom) return false;
        return columnTo == position.columnTo;
    }

    @Override
    public int hashCode() {
        int result = lineFrom;
        result = 31 * result + lineTo;
        result = 31 * result + columnFrom;
        result = 31 * result + columnTo;
        return result;
    }

    @Override
    public String toString() {
        return "Position{" +
                "lineFrom=" + lineFrom +
                ", lineTo=" + lineTo +
                ", columnFrom=" + columnFrom +
                ", columnTo=" + columnTo +
                '}';
    }
}

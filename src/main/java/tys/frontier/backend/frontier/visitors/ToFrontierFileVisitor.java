package tys.frontier.backend.frontier.visitors;

import tys.frontier.code.visitor.FileVisitor;
import tys.frontier.style.Indenter;
import tys.frontier.style.Style;

public class ToFrontierFileVisitor extends FileVisitor.Default<Object, Object, StringBuilder, StringBuilder, StringBuilder> {

    private Style style;
    private Indenter indenter;
    private Appendable out;

    ToFrontierFileVisitor(Style style, Indenter indenter, Appendable out) {
        super(new ToFrontierClassVisitor(style, indenter, out));
        this.style = style;
        this.indenter = indenter;
        this.out = out;
    }

    public static ToFrontierFileVisitor create(Style style, Appendable out) {
        return new ToFrontierFileVisitor(style, style.createIndenter(), out);
    }

}

package tys.frontier.backend.frontier.visitors;

import tys.frontier.code.FClass;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.style.Style;

public class ToFrontierClassVisitor extends ClassVisitor.Default<Object, StringBuilder, StringBuilder, StringBuilder, StringBuilder> {

    private final Style style;

    public ToFrontierClassVisitor(Style style) {
        super(new ToFrontierStatementVisitor(style));
        this.style = style;
    }

    @Override
    public Object enterClass(FClass clazz) {
        return super.enterClass(clazz);
    }
}

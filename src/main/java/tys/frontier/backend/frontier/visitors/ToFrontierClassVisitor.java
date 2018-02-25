package tys.frontier.backend.frontier.visitors;

import tys.frontier.code.*;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.style.Indenter;
import tys.frontier.style.Style;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

//TODO some style params that decide whether we want to leave space between functions/methods

public class ToFrontierClassVisitor extends ClassVisitor.Default<Object, StringBuilder, StringBuilder, StringBuilder, StringBuilder> {

    private final Style style;
    private Indenter indenter;
    private Appendable out;

    ToFrontierClassVisitor(Style style, Indenter indenter, Appendable out) {
        super(new ToFrontierStatementVisitor(style, indenter));
        this.style = style;
        this.indenter = indenter;
        this.out = out;
    }

    @Override
    public Object enterClass(FClass clazz) {
        //header
        StringBuilder sb = new StringBuilder();
        indenter.appendNewLine(sb).append(style.getKeyword(clazz.getVisibility().tokenId)).append(' ').append(style.getKeyword(FrontierParser.CLASS));
        if (style.getOptions().bracketsOnNewLineAfterClass) {
            indenter.appendNewLine(sb).append('{');
        } else {
            sb.append(' ').append('{');
        }
        indenter.increase();
        try {
            out.append(sb);
        } catch (IOException e) {
            e.printStackTrace();
        }

        //body
        List<FClassMember> members = new ArrayList<>();
        members.addAll(clazz.getFields().values());
        members.addAll(clazz.getFunctions().values());
        members.sort(style.getOptions().order);
        for (FClassMember member : members) {
            try {
                if (member instanceof FField)
                    out.append(enterField((FField) member));
                else if (member instanceof FFunction)
                    out.append(enterFunction((FFunction)member).insert(0, indenter.newLine()));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        //footer
        indenter.decrease();
        try {
            out.append(indenter.newLine()).append('}');
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public StringBuilder enterField(FField field) {
        StringBuilder sb = new StringBuilder();
        indenter.appendNewLine(sb).append(style.getKeyword(field.getModifier().tokenId)).append(' ');
        if (field.isStatic())
            sb.append(style.getKeyword(FrontierParser.STATIC)).append(' ');
        //TODO predefined types, keyword clash etc.
        sb.append(field.getType().getIdentifier().name).append(' ').append(field.getIdentifier().name);
        if (field.getAssignment().isPresent()) {
            sb.append(" = ").append(stVis.enterExpression(field.getAssignment().get().getValue()));
        }
        sb.append(';');
        return sb;
    }

    @Override
    public StringBuilder enterFunction(FFunction function) {
        //TODO constructors, operators, predefined types, keyword clash etc...
        StringBuilder sb = new StringBuilder();
        indenter.appendNewLine(sb).append(style.getKeyword(function.getModifier().tokenId)).append(' ');
        if (function.isStatic())
            sb.append(style.getKeyword(FrontierParser.STATIC)).append(' ');
        sb.append(function.getType().getIdentifier().name).append(' ').append(function.getIdentifier().name);
        if (style.getOptions().spaceAfterFunctionName)
            sb.append(' ');
        //params TODO linebreak from maxCharsPer line
        sb.append('(');
        indenter.increase();
        indenter.increase();
        if (style.getOptions().paramsOnNewLine)
            indenter.appendNewLine(sb);

        for (int i=0; i<function.getParams().size(); i++) {
            if (i != 0) {
                sb.append(',').append(' ');
            }
            if(i % style.getOptions().maxParamsPerLine == 0) {
                indenter.appendNewLine(sb);
            }
            FLocalVariable var = function.getParams().get(i);
            //TODO the same as above
            sb.append(var.getType().getIdentifier().name).append(' ').append(var.getIdentifier().name);
        }
        sb.append(')');
        indenter.decrease();
        indenter.decrease();
        //params end

        if (style.getOptions().bracketsOnNewLineAfterFunction)
            indenter.appendNewLine(sb).append('{');
        else
            sb.append(' ').append('{');
        ((ToFrontierStatementVisitor)stVis).appendBlock(sb, function.getBody(), false);

        return sb;
    }
}

package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.InstanceField;

public class NonEmbeddableType extends SyntaxError {

    public final InstanceField field;

    public NonEmbeddableType(InstanceField field) {
        super(field.getLocation().getPoint(), field.getType() + " is not embeddable");
        this.field = field;
    }
}

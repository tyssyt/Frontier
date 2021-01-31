package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.namespace.Namespace;
import tys.frontier.parser.location.Position;

public class NamespaceWithoutType extends TypeNotFound {

    public final Namespace namespace;

    public NamespaceWithoutType(Position position, Namespace namespace) {
        super(position, namespace.getIdentifier());
        this.namespace = namespace;
    }
}
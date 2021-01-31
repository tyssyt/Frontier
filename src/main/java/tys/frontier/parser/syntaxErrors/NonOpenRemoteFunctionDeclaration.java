package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.function.FFunction;
import tys.frontier.code.namespace.Namespace;

public class NonOpenRemoteFunctionDeclaration extends SyntaxError {

    public final FFunction _function;
    public final Namespace remoteNamespace;

    public NonOpenRemoteFunctionDeclaration(FFunction _function, Namespace remoteNamespace, String reason) {
        super(_function.getLocation().getPoint(), remoteNamespace.getLocation().getPoint(), "Invalid remote funcion declaration, remote does not declare signature as open: " + _function);
        this._function = _function;
        this.remoteNamespace = remoteNamespace;
    }
}

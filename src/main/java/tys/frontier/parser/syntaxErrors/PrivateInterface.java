package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FInterface;

public class PrivateInterface extends SyntaxError {

    public final FInterface fInterface;

    public PrivateInterface(FInterface fInterface) {
        super(fInterface.getIdentifier() + " is private");
        this.fInterface = fInterface;
    }
}

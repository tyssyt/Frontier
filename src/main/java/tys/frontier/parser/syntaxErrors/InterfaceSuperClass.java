package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FClass;
import tys.frontier.code.FInterface;

public class InterfaceSuperClass extends SyntaxError {

    public final FInterface fInterface;
    public final FClass superClass;

    public InterfaceSuperClass(FInterface fInterface, FClass superClass) {
        super(fInterface.getIdentifier() + " extends " + superClass.getIdentifier());
        this.fInterface = fInterface;
        this.superClass = superClass;
    }
}

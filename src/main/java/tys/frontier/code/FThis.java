package tys.frontier.code;

import tys.frontier.code.identifier.FVariableIdentifier;

public class FThis extends FVariable {

    private FClass clazz;

    public FThis(FClass clazz) {
        super(FVariableIdentifier.THIS, clazz.getType());
        this.clazz = clazz;
    }
}

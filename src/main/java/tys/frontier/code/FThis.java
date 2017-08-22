package tys.frontier.code;

import tys.frontier.code.identifier.FVariableIdentifier;

public class FThis extends FVariable {

    public FThis(FClass clazz) {
        super(FVariableIdentifier.THIS, clazz);
    }
}

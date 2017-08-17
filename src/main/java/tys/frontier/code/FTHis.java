package tys.frontier.code;

import tys.frontier.code.identifier.FVariableIdentifier;

public class FTHis extends FVariable {

    private FClass clazz;

    public FTHis(FClass clazz) {
        super(FVariableIdentifier.THIS, clazz.getType());
        this.clazz = clazz;
    }
}

package tys.frontier.code.type;

import tys.frontier.code.FClass;

public class FClassType implements FType {

    public final FClass clazz;

    public FClassType(FClass clazz) {
        this.clazz = clazz;
    }

    @Override
    public String toString() {
        return clazz.getIdentifier().toString();
    }
}

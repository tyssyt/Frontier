package tys.frontier.code.type;

import tys.frontier.code.type.capability.ArrayAccessable;

public class FArrayType implements FType, ArrayAccessable {

    public final FType baseType;
    public final int arrayDepth;

    public FArrayType(FType baseType, int arrayDepth) {
        this.baseType = baseType;
        this.arrayDepth = arrayDepth;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder().append(baseType);
        for (int i=0; i<arrayDepth; i++) {
            sb.append('[');
        }
        for (int i=0; i<arrayDepth; i++) {
            sb.append(']');
        }
        return sb.toString();
    }
}

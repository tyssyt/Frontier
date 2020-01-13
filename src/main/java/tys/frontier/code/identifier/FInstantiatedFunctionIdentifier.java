package tys.frontier.code.identifier;

import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.type.FType;

public class FInstantiatedFunctionIdentifier extends FIdentifier {

    public final FIdentifier baseIdentifier;
    public final TypeInstantiation typeInstantiation;

    public FInstantiatedFunctionIdentifier(FIdentifier baseIdentifier, TypeInstantiation typeInstantiation) {
        super(getName(baseIdentifier, typeInstantiation));
        this.baseIdentifier = baseIdentifier;
        this.typeInstantiation = typeInstantiation;
    }

    private static String getName(FIdentifier baseIdentifier, TypeInstantiation typeInstantiation) {
        StringBuilder sb = new StringBuilder(baseIdentifier.name).append('<');
        for (FType type : typeInstantiation.getTypeMap().values()) {
            sb.append(type.getIdentifier().name);
        }
        return sb.append('>').toString();
    }
}

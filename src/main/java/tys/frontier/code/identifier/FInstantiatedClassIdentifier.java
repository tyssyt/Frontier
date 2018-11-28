package tys.frontier.code.identifier;

import tys.frontier.code.FType;
import tys.frontier.code.TypeInstantiation;

public class FInstantiatedClassIdentifier extends FTypeIdentifier {

    public final FTypeIdentifier baseIdentifier;
    public final TypeInstantiation typeInstantiation;

    public FInstantiatedClassIdentifier(FTypeIdentifier baseIdentifier, TypeInstantiation typeInstantiation) {
        super(getName(baseIdentifier, typeInstantiation));
        this.baseIdentifier = baseIdentifier;
        this.typeInstantiation = typeInstantiation;
    }

    private static String getName(FTypeIdentifier baseIdentifier, TypeInstantiation typeInstantiation) {
        StringBuilder sb = new StringBuilder(baseIdentifier.name).append('<');
        for (FType type : typeInstantiation.getTypeMap().values()) {
            sb.append(type.getIdentifier().name);
        }
        return sb.append('>').toString();
    }
}

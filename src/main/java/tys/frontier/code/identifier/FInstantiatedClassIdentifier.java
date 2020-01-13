package tys.frontier.code.identifier;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.type.FType;

public class FInstantiatedClassIdentifier extends FIdentifier {

    public final FIdentifier baseIdentifier;

    public FInstantiatedClassIdentifier(FIdentifier baseIdentifier, ImmutableList<FType> instantiatedParameters) {
        super(getName(baseIdentifier, instantiatedParameters));
        this.baseIdentifier = baseIdentifier;
    }

    private static String getName(FIdentifier baseIdentifier, ImmutableList<FType> instantiatedParameters) {
        StringBuilder sb = new StringBuilder(baseIdentifier.name).append('<');
        for (FType type : instantiatedParameters) {
            sb.append(type.getIdentifier().name);
        }
        return sb.append('>').toString();
    }
}

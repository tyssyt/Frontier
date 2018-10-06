package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.Typed;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public interface FLiteral extends Typed {

    String getOriginalString();

    default FLiteral specify(FClass targetType) throws IncompatibleTypes {
        if (getType() == targetType)
            return this;
        throw new IncompatibleTypes(targetType, getType());
    }

    default int distance(FLiteral other) {
        assert this == other;
        return 0;
    }

}

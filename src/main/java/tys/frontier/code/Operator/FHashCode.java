package tys.frontier.code.Operator;

import tys.frontier.code.FClass;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FInt32;
import tys.frontier.code.predefinedClasses.FPredefinedOperator;

public class FHashCode extends FPredefinedOperator.Unary {
    public FHashCode(FClass clazz) {
        super(FFunctionIdentifier.HASHCODE, clazz, FInt32.INSTANCE);
    }
}

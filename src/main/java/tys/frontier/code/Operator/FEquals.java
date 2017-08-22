package tys.frontier.code.Operator;

import tys.frontier.code.FClass;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.predefinedClasses.FPredefinedOperator;

public class FEquals extends FPredefinedOperator.Binary {
    public FEquals(FClass clazz) {
        super(FFunctionIdentifier.EQUALS, clazz, clazz, FBool.INSTANCE, false);
    }
}
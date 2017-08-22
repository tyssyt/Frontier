package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FClass;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.Operator.FOperator;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;

import java.util.List;

abstract public class FPredefinedOperator extends FOperator {

    public static class Unary extends FPredefinedOperator {
        public Unary(FFunctionIdentifier identifier, FClass clazz, FClass returnType) {
            super(identifier, clazz, returnType, ImmutableList.of(
                    new FLocalVariable(new FVariableIdentifier("object"), clazz)));
        }
        public Unary(FFunctionIdentifier identifier, FClass clazz) {
            this(identifier, clazz, clazz);
        }
    }

    public static class Binary extends FPredefinedOperator {
        public Binary(FFunctionIdentifier identifier, FClass clazz, FClass other, FClass returnType, boolean reverse) {
            super(identifier, clazz, returnType, ImmutableList.of(
                    new FLocalVariable(new FVariableIdentifier("first"), reverse ? other : clazz),
                    new FLocalVariable(new FVariableIdentifier("second"), reverse ? clazz : other)));
        }
        public Binary(FFunctionIdentifier identifier, FClass clazz) {
            this(identifier, clazz, clazz, clazz, false);
        }

    }


    private FPredefinedOperator(FFunctionIdentifier identifier, FClass clazz, FClass returnType, List<FLocalVariable> params) {
        super(identifier, clazz, returnType, params);
        predefined = true;
    }
}

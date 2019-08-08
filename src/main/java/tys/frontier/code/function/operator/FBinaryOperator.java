package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.type.FClass;

public class FBinaryOperator extends FOperator {

    public enum Arith {
        PLUS(new FFunctionIdentifier("+")),
        MINUS(new FFunctionIdentifier("-")),
        TIMES(new FFunctionIdentifier("*")),
        DIVIDED(new FFunctionIdentifier("/")),
        MODULO(new FFunctionIdentifier("%")),
        AND(new FFunctionIdentifier("&")),
        OR(new FFunctionIdentifier("|")),
        XOR(new FFunctionIdentifier("^"));

        public final FFunctionIdentifier identifier;

        Arith(FFunctionIdentifier identifier) {
            this.identifier = identifier;
        }

        public FBinaryOperator create(FClass fClass, FClass returnType, FParameter first, FParameter second) {
            return new FBinaryOperator(identifier, fClass, returnType, first, second);
        }

        public FBinaryOperator createPredefined(FClass fClass) {
            return new FBinaryOperator(identifier, fClass, fClass,
                    FParameter.create(new FVariableIdentifier("first"), fClass, false),
                    FParameter.create(new FVariableIdentifier("second"), fClass, false)) {
                {predefined = true;}
            };
        }

        public FFunction getFunction(FClass fClass) {
            return Iterables.getOnlyElement(fClass.getFunctions().get(identifier));
        }
    }


    public enum Bool {
        AND (new FFunctionIdentifier("&&")),
        OR (new FFunctionIdentifier("||")),
        EQUALS(new FFunctionIdentifier("==")),
        EQUALS_ID(new FFunctionIdentifier("=*=")),
        EQUALS_CONTAINER(new FFunctionIdentifier("=[]=")),
        NOT_EQUALS(new FFunctionIdentifier("=!=")),
        NOT_EQUALS_ID(new FFunctionIdentifier("=!*=")),
        NOT_EQUALS_CONTAINER(new FFunctionIdentifier("=![]=")),
        LESS(new FFunctionIdentifier("<")),
        GREATER(new FFunctionIdentifier(">")),
        LESS_EQUAL(new FFunctionIdentifier("<=")),
        GREATER_EQUAL(new FFunctionIdentifier(">="));

        public final FFunctionIdentifier identifier;

        Bool(FFunctionIdentifier identifier) {
            this.identifier = identifier;
        }

        public FBinaryOperator create(FClass fClass, FParameter first, FParameter second) {
            return new FBinaryOperator(identifier, fClass, FBool.INSTANCE, first, second);
        }

        public FBinaryOperator createPredefined(FClass fClass) {
            return new FBinaryOperator(identifier, fClass, FBool.INSTANCE,
                    FParameter.create(new FVariableIdentifier("first"), fClass, false),
                    FParameter.create(new FVariableIdentifier("second"), fClass, false)) {
                {predefined = true;}
            };
        }

        public FFunction getFunction(FClass fClass) {
            return Iterables.getOnlyElement(fClass.getFunctions().get(identifier));
        }
    }

    private FBinaryOperator(FFunctionIdentifier identifier, FClass fClass, FClass returnType, FParameter first, FParameter second) {
        super(identifier, fClass, returnType, ImmutableList.of(first, second));
        assert fClass == first.getType();
        assert fClass == second.getType();
    }
}

package tys.frontier.code.Operator;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.FType;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FBool;

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

        public FBinaryOperator create(FType fType, FType returnType, FParameter first, FParameter second) {
            return new FBinaryOperator(identifier, fType, returnType, first, second);
        }

        public FBinaryOperator createPredefined(FType fType) {
            return new FBinaryOperator(identifier, fType, fType,
                    new FParameter(new FVariableIdentifier("first"), fType),
                    new FParameter(new FVariableIdentifier("second"), fType)) {
                {predefined = true;}
            };
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

        public FBinaryOperator create(FType fType, FParameter first, FParameter second) {
            return new FBinaryOperator(identifier, fType, FBool.INSTANCE, first, second);
        }

        public FBinaryOperator createPredefined(FType fType) {
            return new FBinaryOperator(identifier, fType, FBool.INSTANCE,
                    new FParameter(new FVariableIdentifier("first"), fType),
                    new FParameter(new FVariableIdentifier("second"), fType)) {
                {predefined = true;}
            };
        }
    }

    private FBinaryOperator(FFunctionIdentifier identifier, FType fType, FType returnType, FParameter first, FParameter second) {
        super(identifier, fType, true, returnType, ImmutableList.of(first, second));
        assert fType == first.getType();
        assert fType == second.getType();
    }
}

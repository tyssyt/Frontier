package tys.frontier.code.Operator;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FType;
import tys.frontier.code.identifier.FFunctionIdentifier;

public class FUnaryOperator extends FOperator {

    public enum Pre {
        NOT(new FFunctionIdentifier("!_")),
        NEG(new FFunctionIdentifier("-_")),
        INC(new FFunctionIdentifier("++_")),
        DEC(new FFunctionIdentifier("--_"));

        public final FFunctionIdentifier identifier;

        Pre(FFunctionIdentifier identifier) {
            this.identifier = identifier;
        }

        public FUnaryOperator create(FType fType) {
            return new FUnaryOperator(identifier, fType, fType);
        }

        public FUnaryOperator createPredefined(FType fType) {
            return new FUnaryOperator(identifier, fType, fType) {
                {predefined = true;}
            };
        }
    }

    public enum Post {
        INC(new FFunctionIdentifier("_++")),
        DEC(new FFunctionIdentifier("_--"));

        public final FFunctionIdentifier identifier;

        Post(FFunctionIdentifier identifier) {
            this.identifier = identifier;
        }

        public FUnaryOperator create(FType fType) {
            return new FUnaryOperator(identifier, fType, fType);
        }

        public FUnaryOperator createPredefined(FType fType) {
            return new FUnaryOperator(identifier, fType, fType) {
                {predefined = true;}
            };
        }
    }

    private FUnaryOperator(FFunctionIdentifier identifier, FType fType, FType returnType) {
        super(identifier, fType, false, returnType, ImmutableList.of());
    }
}

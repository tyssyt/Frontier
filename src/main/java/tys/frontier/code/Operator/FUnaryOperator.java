package tys.frontier.code.Operator;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FClass;
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

        public FUnaryOperator create(FClass fClass) {
            return new FUnaryOperator(identifier, fClass, fClass);
        }

        public FUnaryOperator createPredefined(FClass fClass) {
            return new FUnaryOperator(identifier, fClass, fClass) {
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

        public FUnaryOperator create(FClass fClass) {
            return new FUnaryOperator(identifier, fClass, fClass);
        }

        public FUnaryOperator createPredefined(FClass fClass) {
            return new FUnaryOperator(identifier, fClass, fClass) {
                {predefined = true;}
            };
        }
    }

    private FUnaryOperator(FFunctionIdentifier identifier, FClass fClass, FClass returnType) {
        super(identifier, fClass, false, returnType, ImmutableList.of());
    }
}

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

        public FUnaryOperator create(FClass clazz) {
            return new FUnaryOperator(identifier, clazz, clazz);
        }

        public FUnaryOperator createPredefined(FClass clazz) {
            return new FUnaryOperator(identifier, clazz, clazz) {
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

        public FUnaryOperator create(FClass clazz) {
            return new FUnaryOperator(identifier, clazz, clazz);
        }

        public FUnaryOperator createPredefined(FClass clazz) {
            return new FUnaryOperator(identifier, clazz, clazz) {
                {predefined = true;}
            };
        }
    }

    private FUnaryOperator(FFunctionIdentifier identifier, FClass clazz, FClass returnType) {
        super(identifier, clazz, false, returnType, ImmutableList.of());
    }
}

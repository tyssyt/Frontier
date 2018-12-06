package tys.frontier.code.Operator;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.FParameter;
import tys.frontier.code.FType;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;

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

        public FFunction getFunction(FType type) {
            return Iterables.getOnlyElement(type.getFunctions().get(identifier));
        }
    }

    private FUnaryOperator(FFunctionIdentifier identifier, FClass fClass, FClass returnType) {
        super(identifier, fClass, returnType, ImmutableList.of(FParameter.create(FVariableIdentifier.THIS, fClass, false)));
    }
}

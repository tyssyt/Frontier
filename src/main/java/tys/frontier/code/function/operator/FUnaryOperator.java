package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.type.FClass;

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

        public FFunction getFunction(FClass fClass) {
            return Iterables.getOnlyElement(fClass.getFunctions().get(identifier));
        }
    }

    private FUnaryOperator(FFunctionIdentifier identifier, FClass fClass, FClass returnType) {
        super(identifier, fClass, returnType, ImmutableList.of(FParameter.create(FVariableIdentifier.THIS, fClass, false)));
    }
}

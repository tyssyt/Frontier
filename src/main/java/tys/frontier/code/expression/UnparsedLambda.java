package tys.frontier.code.expression;

import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.InvalidExpressionBinding;
import tys.frontier.parser.syntaxTree.ToInternalRepresentation;
import tys.frontier.util.Utils;

import java.util.List;
import java.util.Optional;

import static java.util.stream.Collectors.toUnmodifiableList;
import static tys.frontier.util.Utils.typesFromExpressionList;

public class UnparsedLambda extends UnboundExpression {

    private FIdentifier identifier;
    private FFunctionType type;
    private List<FTypeVariable> in;

    private ToInternalRepresentation parser;
    private FrontierParser.LambdaContext context;

    private FBaseFunction parsedAs;

    public UnparsedLambda(Position position, FIdentifier identifier, FFunctionType type, List<FTypeVariable> in, ToInternalRepresentation parser, FrontierParser.LambdaContext context) {
        super(position);
        this.identifier = identifier;
        this.type = type;
        this.in = in;
        this.parser = parser;
        this.context = context;
    }

    @Override
    public FFunctionType getType() {
        return type;
    }

    @Override
    public List<FTypeVariable> getIn() {
        return in;
    }

    @Override
    public Optional<FTypeVariable> getOut() {
        return Optional.of((FTypeVariable) type.getOut());
    }

    @Override
    public FExpression bind(TypeInstantiation typeInstantiation) throws InvalidExpressionBinding {
        assert typeInstantiation.keys().containsAll(in);
        List<FType> newTypes = FTuple.unpackType(type.getIn()).stream().map(typeInstantiation::getType).collect(toUnmodifiableList());
        return bind(FFunctionType.from(FTuple.from(newTypes), typeInstantiation.getType(this.type.getOut())));
    }

    @Override
    public FExpression bind(FType targetType) throws InvalidExpressionBinding {
        assert targetType instanceof FFunctionType;
        if (getType() == targetType && !in.isEmpty())
            throw new InvalidExpressionBinding(this, targetType);

        List<FType> newIn = FTuple.unpackType(((FFunctionType) targetType).getIn());
        FType newOut = ((FFunctionType) targetType).getOut();
        if (newOut == this.type.getOut())
            newOut = null;

        if (parsedAs == null) {
            parsedAs = parser.visitLambda(context, identifier, newIn, newOut);
            parser = null;
            context = null;
        } else {
            // lambdas can only be bound once, but if we request the same binding we can just return the old result
            if (!newIn.equals(typesFromExpressionList(parsedAs.getSignature().getParameters())))
                return Utils.NYI("binding lambda multiple times"); //TODO proper error
            if (newOut != null && newOut != parsedAs.getType())
                return Utils.NYI("binding lambda multiple times"); //TODO proper error
        }

        return new FFunctionAddress(getPosition(), parsedAs);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(identifier);
    }
}

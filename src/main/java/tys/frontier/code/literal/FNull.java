package tys.frontier.code.literal;

import com.google.common.collect.ListMultimap;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.util.Utils;

import java.util.List;

public class FNull implements FLiteral {

    private static final FTypeIdentifier IDENTIFIER = new FTypeIdentifier("!NullType");
    public static FType NULL_TYPE = new FType() {

        @Override
        public long concreteness() {
            return Long.MAX_VALUE;
        }

        @Override
        public boolean canImplicitlyCast() {
            return false;
        }

        @Override
        public FunctionResolver.Result softResolveFunction(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve) {
            return Utils.cantHappen();
        }

        @Override
        public FTypeIdentifier getIdentifier() {
            return IDENTIFIER;
        }

        @Override
        public ForImpl getForImpl() {
            return null;
        }

        @Override
        public StringBuilder toString(StringBuilder sb) {
            return Utils.cantHappen();
        }
    };
    public static final FNull UNTYPED = new FNull(NULL_TYPE);

    private FType type;

    public FNull(FType type) {
        assert type == NULL_TYPE || FOptional.canBeTreatedAsOptional(type);
        this.type = type;
    }

    @Override
    public FLiteral copy() {
        return UNTYPED;
    }

    @Override
    public FType getType() {
        return type;
    }

    @Override
    public String toString() {
        return "null";
    }

    @Override
    public String getOriginalString() {
        return toString();
    }
}

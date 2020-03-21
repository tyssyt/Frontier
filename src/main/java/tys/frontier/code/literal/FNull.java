package tys.frontier.code.literal;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.type.FType;
import tys.frontier.util.Utils;

public class FNull implements FLiteral {

    private static final FIdentifier IDENTIFIER = new FIdentifier("!NullType");
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
        public Namespace getNamespace() {
            return Utils.cantHappen();
        }

        @Override
        public FIdentifier getIdentifier() {
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

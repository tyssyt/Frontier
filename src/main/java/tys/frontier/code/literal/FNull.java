package tys.frontier.code.literal;

import com.google.common.collect.Multimap;
import tys.frontier.code.FField;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.util.Utils;

import java.util.List;

public class FNull implements FLiteral {

    private static final FTypeIdentifier IDENTIFIER = new FTypeIdentifier("!NullType");
    public static FType NULL_TYPE = new FType() {

        @Override
        public FField getField(FIdentifier identifier)  {
            return Utils.cantHappen();
        }

        @Override
        public long concreteness() {
            return Long.MAX_VALUE;
        }

        @Override
        public boolean canImplicitlyCast() {
            return false;
        }

        @Override
        public FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> argumentTypes, FType returnType, TypeInstantiation typeInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints) {
            return Utils.cantHappen();
        }

        @Override
        public FTypeIdentifier getIdentifier() {
            return IDENTIFIER;
        }

        @Override
        public StringBuilder toString(StringBuilder sb) {
            return Utils.cantHappen();
        }
    };
    public static final FNull UNTYPED = new FNull(null);

    private FOptional type;

    public FNull(FOptional type) {
        this.type = type;
    }

    @Override
    public FLiteral copy() {
        return UNTYPED;
    }

    @Override
    public FType getType() {
        return type == null ? NULL_TYPE : type;
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

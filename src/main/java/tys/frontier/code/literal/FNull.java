package tys.frontier.code.literal;

import com.google.common.collect.Multimap;
import tys.frontier.code.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.util.Utils;

import java.util.List;

public class FNull implements FLiteral {

    public static FType NULL_TYPE = new FType() {
        @Override
        public FField getField(FIdentifier identifier)  {
            return Utils.cantHappen();
        }

        @Override
        public boolean isFullyInstantiated() {
            return true;
        }

        @Override
        public FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> argumentTypes, TypeInstantiation typeInstantiation) {
            return Utils.cantHappen();
        }
        @Override
        public FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> argumentTypes, TypeInstantiation typeInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints) {
            return Utils.cantHappen();
        }

        @Override
        public FTypeIdentifier getIdentifier() {
            return Utils.cantHappen();
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

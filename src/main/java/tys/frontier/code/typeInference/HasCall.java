package tys.frontier.code.typeInference;

import com.google.common.collect.ListMultimap;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;

import java.util.List;

public class HasCall extends TypeConstraint {

    //TODO can there be a case where we know the return Type and want a member for that?
    private FFunctionIdentifier identifier;
    private List<FType> positionalArgs;
    private ListMultimap<FIdentifier, FType> keywordArgs;
    private boolean lhsResolve;

    public HasCall(FExpression origin, FFunctionIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, boolean lhsResolve) {
        super(origin);
        this.identifier = identifier;
        this.positionalArgs = positionalArgs;
        this.keywordArgs = keywordArgs;
        this.lhsResolve = lhsResolve;
    }

    public FFunctionIdentifier getIdentifier() {
        return identifier;
    }

    public List<FType> getPositionalArgs() {
        return positionalArgs;
    }

    public ListMultimap<FIdentifier, FType> getKeywordArgs() {
        return keywordArgs;
    }

    public boolean isLhsResolve() {
        return lhsResolve;
    }

    @Override
    public String toString() {
        return "HasCall{" + identifier + '(' + positionalArgs + keywordArgs + ")}";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof HasCall)) return false;

        HasCall hasCall = (HasCall) o;

        if (!identifier.equals(hasCall.identifier)) return false;
        if (!positionalArgs.equals(hasCall.positionalArgs)) return false;
        return keywordArgs.equals(hasCall.keywordArgs);
    }

    @Override
    public int hashCode() {
        int result = identifier.hashCode();
        result = 31 * result + positionalArgs.hashCode();
        result = 31 * result + keywordArgs.hashCode();
        return result;
    }
}

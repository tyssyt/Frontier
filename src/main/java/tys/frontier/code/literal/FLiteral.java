package tys.frontier.code.literal;

import tys.frontier.code.Typed;

public interface FLiteral extends Typed {

    FLiteral copy();
    String getOriginalString();
}

package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.function.FFunction;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;

public class SignatureCollision extends SyntaxError {

    public final FFunction a;
    public final FFunction b;

    public SignatureCollision(FFunction a, FFunction b) {
        super("between " + a.headerToString() + " and " + b.headerToString());
        this.a = a;
        this.b = b;
    }

    public static boolean collide(FFunction first, FFunction second) {
        //see of one of the functions has more parameters
        int min = Math.min(first.getParams().size(), second.getParams().size());
        if (first.getParams().size() > min && !first.getParams().get(min).hasDefaultValue())
            return false;
        if (second.getParams().size() > min && !second.getParams().get(min).hasDefaultValue())
            return false;

        //try to find a arg that is different
        for (int i=0; i < min; i++) {
            FType t =  first.getParams().get(i).getType();
            FType o = second.getParams().get(i).getType();
            //args are not different if they are a type variable
            if (!(t instanceof FTypeVariable) && !(o instanceof FTypeVariable) && t != o)
                return false;
        }
        return true;
    }
}

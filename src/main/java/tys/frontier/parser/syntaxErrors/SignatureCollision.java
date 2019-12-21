package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.Signature;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;

public class SignatureCollision extends SyntaxError {

    public final Signature a;
    public final Signature b;

    public SignatureCollision(Signature a, Signature b) {
        super("between " + a + " and " + b);
        this.a = a;
        this.b = b;
    }

    public static boolean collide(Signature first, Signature second) {
        ImmutableList<FParameter> firstParams  = first.getParameters();
        ImmutableList<FParameter> secondParams = second.getParameters();

        //see of one of the functions has more parameters
        int min = Math.min(firstParams.size(), secondParams.size());
        if (firstParams.size() > min && !firstParams.get(min).hasDefaultValue())
            return false;
        if (secondParams.size() > min && !secondParams.get(min).hasDefaultValue())
            return false;

        //try to find a arg that is different
        for (int i=0; i < min; i++) {
            FType t =  firstParams.get(i).getType();
            FType o = secondParams.get(i).getType();
            //args are not different if they are a type variable
            if (!(t instanceof FTypeVariable) && !(o instanceof FTypeVariable) && t != o)
                return false;
        }
        return true;
    }
}

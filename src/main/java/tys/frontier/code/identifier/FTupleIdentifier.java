package tys.frontier.code.identifier;

import tys.frontier.code.type.FType;
import tys.frontier.util.Utils;

import java.util.List;

public class FTupleIdentifier extends FTypeIdentifier {

    public FTupleIdentifier(List<FType> types) {
        super(getName(types));
    }

    public static String getName(List<FType> in) {
        StringBuilder sb = new StringBuilder().append('(');
        Utils.joinIdentifiers(sb, in, " ");
        return sb.append(')').toString();
    }
}

package tys.frontier.code.identifier;

import tys.frontier.code.type.FType;
import tys.frontier.util.Utils;

import java.util.List;

public class FFunctionTypeIdentifier extends FTypeIdentifier {

    public FFunctionTypeIdentifier(List<FType> in, FType out) {
        super(getName(in, out));
    }

    public static String getName(List<FType> in, FType out) {
        StringBuilder sb = new StringBuilder();
        Utils.joinIdentifiers(sb, in, ", ");
        sb.append(" -> ").append(out.getIdentifier());
        return sb.toString();
    }
}
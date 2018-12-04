package tys.frontier.code.identifier;

import tys.frontier.code.FType;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.util.Utils;

import java.util.List;

public class FFunctionTypeIdentifier extends FTypeIdentifier {

    public FFunctionTypeIdentifier(List<FType> in, FType out) {
        super(getName(in, out));
    }

    public static String getName(List<FType> in, FType out) {
        StringBuilder sb = new StringBuilder();
        Utils.joinIdentifiers(sb, in, ", ");
        if (out != FVoid.INSTANCE)
            sb.append(" -> ").append(out.getIdentifier());
        return sb.toString();
    }
}
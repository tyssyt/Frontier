package tys.frontier.code.identifier;

import tys.frontier.code.type.FType;

public class FFunctionTypeIdentifier extends FIdentifier {

    public FFunctionTypeIdentifier(FType in, FType out) {
        super("(" + in.getIdentifier() + " -> " + out.getIdentifier() + ')');
    }
}
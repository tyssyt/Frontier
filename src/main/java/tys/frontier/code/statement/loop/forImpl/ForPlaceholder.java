package tys.frontier.code.statement.loop.forImpl;

import tys.frontier.code.type.FType;
import tys.frontier.util.Utils;

public class ForPlaceholder implements ForImpl {

    public static ForImpl INSTANCE = new ForPlaceholder();

    private ForPlaceholder() {}

    @Override
    public FType getElementType() {
        return Utils.cantHappen();
    }
}

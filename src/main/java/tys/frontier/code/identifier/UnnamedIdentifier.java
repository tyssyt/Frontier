package tys.frontier.code.identifier;

public class UnnamedIdentifier extends FIdentifier {

    public static UnnamedIdentifier get() {
        return new UnnamedIdentifier();
    }

    protected UnnamedIdentifier() {
        super("!_");
    }

    @Override
    public boolean equals(Object o) {
        return false;
    }
}

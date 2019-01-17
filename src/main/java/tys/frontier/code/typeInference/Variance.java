package tys.frontier.code.typeInference;

public enum Variance {
    Covariant(1),
    Contravariant(-1),
    Invariant(0);

    public final int sign;

    Variance(int sign) {
        this.sign = sign;
    }

    public static Variance fromSign(int sign) {
        if (sign == 0)
            return Invariant;
        if (sign > 0)
            return Covariant;
        return Contravariant;
    }

    public Variance then(Variance other) {
        return fromSign(this.sign * other.sign);
    }
}
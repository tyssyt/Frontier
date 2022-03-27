package tys.frontier.code.typeInference;

public enum Variance {
    Covariant(1, '>'),
    Contravariant(-1, '<'),
    Invariant(0, '=');

    public final int sign;
    public final char _char;

    Variance(int sign, char _char) {
        this.sign = sign;
        this._char = _char;
    }

    public static Variance fromSign(int sign) {
        if (sign == 0)
            return Invariant;
        if (sign > 0)
            return Covariant;
        return Contravariant;
    }

    public Variance opposite() {
        return switch (this) {
            case Covariant -> Contravariant;
            case Contravariant -> Covariant;
            case Invariant -> Invariant;
        };
    }

    public Variance then(Variance other) {
        return fromSign(this.sign * other.sign); //TODO isn't this completely wrong? like contra then contra shouldn't be co?
    }
}
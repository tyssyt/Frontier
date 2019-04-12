package tys.frontier.code.typeInference;

import tys.frontier.util.Utils;

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

    public Variance opposite() {
        switch (this) {
            case Covariant:     return Contravariant;
            case Contravariant: return Covariant;
            case Invariant:     return Invariant;
            default:            return Utils.cantHappen();
        }
    }

    public Variance then(Variance other) {
        return fromSign(this.sign * other.sign);
    }
}
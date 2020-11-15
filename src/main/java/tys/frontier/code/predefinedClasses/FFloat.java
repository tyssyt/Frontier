package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FIdentifier;

public abstract class FFloat extends FPredefinedClass {

    public static FIdentifier LOG = new FIdentifier("log");
    public static FIdentifier LOG10 = new FIdentifier("log10");
    public static FIdentifier LOG2 = new FIdentifier("log2");
    public static FIdentifier CEIL = new FIdentifier("ceil");
    public static FIdentifier FLOOR = new FIdentifier("floor");
    public static FIdentifier TRUNC = new FIdentifier("trunc");
    public static FIdentifier RAW_BITS = new FIdentifier("asRawBits");
    public static FIdentifier SPLIT_REPRESENTATION = new FIdentifier("splitRepresentation");

    protected FFloat(FIdentifier identifier) {
        super(identifier);
    }

    public abstract int getBits();
}

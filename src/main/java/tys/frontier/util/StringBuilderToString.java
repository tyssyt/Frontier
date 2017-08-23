package tys.frontier.util;

public interface StringBuilderToString {

    StringBuilder toString(StringBuilder sb);

    default String tS() {
        return toString(new StringBuilder()).toString();
    }
}

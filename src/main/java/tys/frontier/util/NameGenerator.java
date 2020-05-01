package tys.frontier.util;

import java.util.Arrays;
import java.util.Iterator;

public class NameGenerator implements Iterator<String> {

    private String prefix;
    private String suffix;
    private char[] infix = new char[] {'a'};

    public NameGenerator(String prefix, String suffix) {
        this.prefix = prefix;
        this.suffix = suffix;
    }

    public static int infixIndex(String infix) {
        int res = 0;
        for (char c : infix.toCharArray()) {
            res *= 26*2;
            if ('a' <= c && c <= 'z') {
                res += 1 + c-'a';
            } else if ('A' <= c && c <= 'Z') {
                res += 27 + c-'A';
            } else {
                return Utils.cantHappen();
            }
        }
        return res-1;
    }

    @Override
    public boolean hasNext() {
        return true;
    }

    @Override
    public String next() {
        String res = prefix + String.valueOf(infix) + suffix;
        inc();
        return res;
    }

    private void inc() {
        for (int i = infix.length - 1; i >= 0; i--) {
            infix[i]++;
            if (infix[i] == '{')
                infix[i] = 'A';
            if (infix[i] != '[')
                break;
            infix[i] = 'a';
            if (i==0)
                append();
        }
    }

    private void append() {
        infix = new char[infix.length+1];
        Arrays.fill(infix, 'a');
    }
}

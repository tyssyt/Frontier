package tys.frontier.util;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import static java.math.BigInteger.ONE;
import static java.math.BigInteger.TEN;
import static org.junit.Assert.assertEquals;

public class FloatTableStuff {

    static final int SCALE = 10000;

    static MathContext context = new MathContext(1000);
    static BigInteger hurrDurr = new BigInteger("347063955532709820");
    static BigInteger _64thBitSet = ONE.shiftLeft(63);
    static BigInteger _65thBitSet = ONE.shiftLeft(64);
    static BigInteger shiftedOne = ONE.shiftLeft(60);

    //@Test
    public void testAsdasd() {
        try (FileWriter fileWriter = new FileWriter("table1.txt");
             BufferedWriter bufferedWriter = new BufferedWriter(fileWriter))
        {
            for (int i = -126; i <= 127 ; i++) {
                Triple<Integer, Long, Long> triple = doSthmElse(i);
                String format = String.format("table[%d] = %d, %d, %d;\n", i + 127, triple.a, triple.b, triple.c);
                System.out.print(format);
                bufferedWriter.write(format);

            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    //@Test
    public void testIt() {

        for (int i = -126; i <= 127 ; i++) {
            Triple<Integer, Long, Long> triple = doSthmElse(i);

            //build some strings
            /*
            StringBuilder mantissaBits = new StringBuilder();
            for (byte b : triple.b.toByteArray()) {
                mantissaBits.append(String.format("%32s", Integer.toBinaryString(b)).substring(24).replace(' ', '0'));
            }
            */

            BigDecimal magic = new BigDecimal(Long.toUnsignedString(triple.b));
            magic = magic.divide(BigDecimal.valueOf(2).pow(60));

            String actualMan = String.format("%.8f", magic);

            double target = Math.pow(2, i);
            String expected = String.format("%.8e", target);

            String[] es = expected.split("e");
            int eEx = Integer.parseInt(es[1]);

            /*
            System.out.println(magic);
            System.out.println(target);
            System.out.println();
             */

            assertEquals(es[0], actualMan);
            assertEquals(eEx, triple.a.longValue());
        }

    }


    static Triple<Integer, Long, Long> doSthmElse(int binExp) {
        BigInteger decExp = hurrDurr.multiply(BigInteger.valueOf(binExp));

        BigInteger effectiveExp = decExp.divide(shiftedOne);

        BigInteger aTHing;
        if (binExp >= 0) {
            aTHing = ONE.shiftLeft(binExp + 37).add(ONE).divide(TEN.pow(effectiveExp.intValueExact()));
        } else {
            effectiveExp = effectiveExp.subtract(ONE);
            aTHing = ONE.shiftLeft(37).multiply(TEN.pow(effectiveExp.negate().intValueExact())).divide(ONE.shiftLeft(-binExp));
        }

        BigInteger bTHing = aTHing.divide(TEN);

        if (aTHing.compareTo(_64thBitSet) >= 0) {
            //poor mens twos complement
            aTHing = _65thBitSet.subtract(aTHing).negate();
        }

        return new Triple<>(effectiveExp.intValueExact(), aTHing.longValueExact(), bTHing.longValueExact());
    }


}
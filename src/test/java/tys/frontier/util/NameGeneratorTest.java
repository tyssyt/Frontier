package tys.frontier.util;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class NameGeneratorTest {

    @Test
    public void testNameGenerator() {
        NameGenerator nameGenerator = new NameGenerator("", "");

        for (int i = 0; i < 10000; i++) {
            String name = nameGenerator.next();
            int nameIdx = NameGenerator.infixIndex(name);
            assertEquals(i, nameIdx);
        }
    }
}
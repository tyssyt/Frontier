package tys.frontier.main;

import com.google.common.io.CharStreams;
import org.junit.Before;
import org.junit.Test;
import tys.frontier.logging.Log;
import tys.frontier.logging.Logger;
import tys.frontier.logging.StdOutLogger;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Random;

import static org.junit.Assert.assertEquals;

public class MainTest {

    private static final String prefix = "Parser/Main/";
    private static final String out = "tmp/";

    @Before
    public void setUp() {
        for (File file : new File("tmp/").listFiles()) {
            file.delete();
        }

        Logger logger = Log.DEFAULT_LOGGER;
        if (logger instanceof StdOutLogger)
            ((StdOutLogger) logger).setLevel(Logger.Level.WARNING);
    }

    private String doMain(String fileName, String input) throws IOException, InterruptedException, SyntaxErrors {
        Main.main(prefix + fileName + ".front", out + fileName);
        Process p = new ProcessBuilder(out + fileName + ".exe").start();
        if (input != null) {
            OutputStreamWriter writer = new OutputStreamWriter(p.getOutputStream());
            writer.write(input);
            writer.flush();
        }
        p.waitFor();
        return CharStreams.toString(new InputStreamReader(p.getInputStream()));
    }

    @Test
    public void mainHelloWorld() throws IOException, InterruptedException, SyntaxErrors {
        String res = doMain("HelloWorld", null);
        assertEquals("Hello, World!\r\n", res); //TODO I wish I could fix whoever adds the \r in there...
    }
    @Test
    public void mainEcho() throws IOException, InterruptedException, SyntaxErrors {
        Random r = new Random();
        StringBuilder sb = new StringBuilder();
        for (int i=0; i<255; i++) {
            char c = (char)(32 + r.nextInt(95));
            sb.append(c);
        }
        String test = sb.toString();

        String res = doMain("Echo", test + '\n');
        assertEquals(test, res);
    }
    @Test
    public void mainDelegate() throws IOException, InterruptedException, SyntaxErrors {
        String res = doMain("Delegate", null);
        assertEquals("this is from A,castSuccessfull!", res);
    }
    @Test
    public void mainOptional() throws IOException, InterruptedException, SyntaxErrors {
        String res = doMain("Optional", null);
        assertEquals("null pexists null qexists q", res);
    }
}
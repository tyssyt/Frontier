package tys.frontier.main;

import com.google.common.base.Charsets;
import com.google.common.collect.ImmutableList;
import com.google.common.io.CharStreams;
import com.google.common.io.Resources;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import tys.frontier.logging.Log;
import tys.frontier.logging.Logger;
import tys.frontier.logging.StdOutLogger;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.util.FileUtils;
import tys.frontier.util.Utils;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Random;

import static org.junit.Assert.*;

public class MainTest {

    private static final String prefix = "Parser" + Utils.filesep + "Main" + Utils.filesep;

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @BeforeClass
    public static void setUp() {
        Logger logger = Log.DEFAULT_LOGGER;
        if (logger instanceof StdOutLogger)
            ((StdOutLogger) logger).setLevel(Logger.Level.INFO);
    }

    private String doMain(String fileName, String input, String... args) throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String tmpFolder = this.folder.newFolder().getPath() + Utils.filesep;

        Main.main(FileUtils.pathToResource(prefix + fileName + ".front").toString(), tmpFolder, new ArrayList<>(), false);

        //we need to redirect the output to a file, because Java can't handle storing large outputs, and we can peek it
        ImmutableList<String> processCall = ImmutableList.<String>builder().add(tmpFolder + ".exe").add(args).build();
        File output = new File(tmpFolder + ".txt");
        Process p = new ProcessBuilder(processCall).redirectOutput(output).start();

        if (input != null) {
            try (OutputStreamWriter writer = new OutputStreamWriter(p.getOutputStream())) {
                writer.write(input);
                writer.flush();
            }
        }
        p.waitFor();
        try (FileReader reader = new FileReader(output)) {
            return CharStreams.toString(reader);
        }
    }

    private static String loadOut(String fileName) throws IOException {
        URL url = Resources.getResource(prefix + fileName);
        return Resources.toString(url, Charsets.UTF_8).replaceAll("\r?\n", Utils.endl);
    }

    @Test
    public void mainHelloWorld() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("HelloWorld", null);
        assertEquals("Hello, World!" + Utils.endl, res);
    }
    @Test
    public void mainEcho() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
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
    public void mainDelegate() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("Delegate", null);
        assertEquals("this is from A,castSuccessfull!", res);
    }
    @Test
    public void mainOptional() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("Optional", null);
        assertEquals("null pexists null qexists qqqlambdaIf", res);
    }
    @Test
    public void mainGeneric() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("Generic", null);
        assertEquals("ayeney", res);
    }
    @Test
    public void mainOptGeneric() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("OptGeneric", null);
        assertEquals("024568", res);
    }
    @Test
    public void mainHigherOrder() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("HigherOrder", null);
        StringBuilder expected = new StringBuilder();
        for(int i=1; i<=5; i++) {
            expected.append(i).append(":0123456789101112").append(Utils.endl);
        }
        expected.append("done");
        assertEquals(expected.toString(), res);
    }
    @Test
    public void mainLambda() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("Lambda", null);
        assertEquals(loadOut("LambdaOut.txt"), res);
    }
    @Test
    public void mainDelegateGeneric() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("DelegateGeneric", null);
        assertEquals("1098|116|10985151|59|onetwo", res);
    }
    @Test
    public void mainParams() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("Params", ";");
        assertEquals("20@#;" + Utils.endl, res);
    }
    @Test
    public void mainPackUnpack() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("PackUnpack", "abc");
        assertEquals(loadOut("PackUnpackOut.txt"), res);
    }
    @Test
    public void mainTupleInstantiations() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("TupleInstantiations", null);
        assertEquals(loadOut("TupleInstantiationsOut.txt"), res);
    }
    @Test
    public void mainPrimes() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("Primes", null, "10000");
        assertEquals(loadOut("PrimesOut.txt"), res);
    }
    @Test
    public void mainInclude() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("Include/Include", null);
        assertEquals("Hello" + Utils.endl + "There!" + Utils.endl, res);
    }
    @Test
    public void mainNativeInclude() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("NativeInclude", null);
        assertEquals("42", res);
    }
    @Test
    public void mainOperators() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("Operators", null);
        assertEquals(loadOut("OperatorsOut.txt"), res);
    }
    @Test
    public void mainBreaksOptimizer() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("BreaksOptimizer", null);
        assertEquals("00", res);
    }
    @Test
    public void mainSort() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("Sort", null, "50000", "100");
        System.out.println(res);
        assertTrue(res.contains("ordered"));
        assertFalse(res.contains("FAILED"));
    }
    @Test
    public void mainTypeInfo() throws IOException, InterruptedException, SyntaxErrors, SyntaxError {
        String res = doMain("TypeInfo", null);
        assertEquals(loadOut("TypeInfoOut.txt"), res);
    }
}
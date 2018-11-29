package tys.frontier.parser;

import com.google.common.collect.Iterables;
import org.junit.Before;
import org.junit.Test;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.logging.Log;
import tys.frontier.logging.Logger;
import tys.frontier.logging.StdOutLogger;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.style.Style;

import java.io.IOException;

import static org.junit.Assert.assertTrue;

public class ParserTest {

    private static final String prefix = "Parser/SyntaxErrors/";

    @Before
    public void setUp() {
        Logger logger = Log.DEFAULT_LOGGER;
        if (logger instanceof StdOutLogger)
            ((StdOutLogger) logger).setLevel(Logger.Level.WARNING);
    }

    private SyntaxError parseSyntaxError(String file) throws IOException {
        try {
            new Parser(prefix + file, Style.DEFAULT_STYLE).parse();
        } catch (SyntaxErrors es) {
            return Iterables.getOnlyElement(es.errors);
        }
        throw new RuntimeException("no error");
    }

    @Test
    public void parseAccessForbidden() throws Exception {
        SyntaxError e = parseSyntaxError("AccessForbidden.front");
        assertTrue(e instanceof AccessForbidden);
        assertTrue(((AccessForbidden) e).accessed instanceof FField);
    }
    @Test
    public void parseBreakOutsideLoop() throws Exception {
        SyntaxError e = parseSyntaxError("BreakOutsideLoop.front");
        assertTrue(e instanceof StatementOutsideLoop);
    }
    @Test
    public void parseClassIdentifierCollision() throws Exception {
        SyntaxError e = parseSyntaxError("ClassIdentifierCollision.front");
        assertTrue(e instanceof IdentifierCollision);
        IdentifierCollision c = ((IdentifierCollision) e);
        assertTrue(c.a instanceof FClass);
        assertTrue(c.b instanceof FClass);
    }
    @Test
    public void parseContinueOutsideLoop() throws Exception {
        SyntaxError e = parseSyntaxError("ContinueOutsideLoop.front");
        assertTrue(e instanceof StatementOutsideLoop);
    }
    @Test
    public void parseCyclicDelegate() throws Exception {
        SyntaxError e = parseSyntaxError("CyclicDelegate.front");
        assertTrue(e instanceof CyclicDelegate);
    }
    @Test
    public void parseCyclicDelegate2() throws Exception {
        SyntaxError e = parseSyntaxError("CyclicDelegate2.front");
        assertTrue(e instanceof CyclicDelegate);
    }
    @Test
    public void parseFieldIdentifierCollision() throws Exception {
        SyntaxError e = parseSyntaxError("FieldIdentifierCollision.front");
        assertTrue(e instanceof IdentifierCollision);
        IdentifierCollision c = ((IdentifierCollision) e);
        assertTrue(c.a instanceof FField);
        assertTrue(c.b instanceof FField);
    }
    @Test
    public void parseFieldNotFound() throws Exception {
        SyntaxError e = parseSyntaxError("FieldNotFound.front");
        assertTrue(e instanceof FieldNotFound);
    }
    @Test
    public void parseFunctionNotFound() throws Exception {
        SyntaxError e = parseSyntaxError("FunctionNotFound.front");
        assertTrue(e instanceof FunctionNotFound);
    }
    @Test
    public void parseIncompatibleTypesAssignment() throws Exception {
        SyntaxError e = parseSyntaxError("IncompatibleTypesAssignment.front");
        assertTrue(e instanceof IncompatibleTypes);
    }
    @Test
    public void parseIncompatibleTypesFor() throws Exception {
        SyntaxError e = parseSyntaxError("IncompatibleTypesFor.front");
        assertTrue(e instanceof IncompatibleTypes);
    }
    @Test
    public void parseIncompatibleTypesIf() throws Exception {
        SyntaxError e = parseSyntaxError("IncompatibleTypesIf.front");
        assertTrue(e instanceof IncompatibleTypes);
    }
    @Test
    public void parseIncompatibleTypesReturn() throws Exception {
        SyntaxError e = parseSyntaxError("IncompatibleTypesReturn.front");
        assertTrue(e instanceof IncompatibleTypes);
    }
    @Test
    public void parseIncompatibleTypesWhile() throws Exception {
        SyntaxError e = parseSyntaxError("IncompatibleTypesWhile.front");
        assertTrue(e instanceof IncompatibleTypes);
    }
    @Test
    public void parseIntLiteralTooLarge() throws Exception {
        SyntaxError e = parseSyntaxError("IntLiteralTooLarge.front");
        assertTrue(e instanceof IntLiteralTooLarge);
    }
    @Test
    public void parseNonOptionalExMark() throws Exception {
        SyntaxError e = parseSyntaxError("NonOptionalExMark.front");
        assertTrue(e instanceof NonOptionalExMark);
    }
    @Test
    public void parseParameterizedTypeVariable() throws Exception {
        SyntaxError e = parseSyntaxError("ParameterizedTypeVariable.front");
        assertTrue(e instanceof ParameterizedTypeVariable);
    }
    @Test
    public void parseSignatureCollision() throws Exception {
        SyntaxError e = parseSyntaxError("SignatureCollision.front");
        assertTrue(e instanceof SignatureCollision);
    }
    @Test
    public void parseTwiceDefinedLocalVariable() throws Exception {
        SyntaxError e = parseSyntaxError("TwiceDefinedLocalVariable.front");
        assertTrue(e instanceof TwiceDefinedLocalVariable);
    }
    @Test
    public void parseTypeNotFound() throws Exception {
        SyntaxError e = parseSyntaxError("TypeNotFound.front");
        assertTrue(e instanceof TypeNotFound);
    }
    @Test
    public void parseUndeclaredVariable() throws Exception {
        SyntaxError e = parseSyntaxError("UndeclaredVariable.front");
        assertTrue(e instanceof UndeclaredVariable);
    }
    @Test
    public void parseUntypedVariable() throws Exception {
        SyntaxError e = parseSyntaxError("UntypedVariable.front");
        assertTrue(e instanceof UntypedVariable);
    }
    @Test
    public void parseWrongNumberOfTypeArguments() throws Exception {
        SyntaxError e = parseSyntaxError("WrongNumberOfTypeArguments.front");
        assertTrue(e instanceof WrongNumberOfTypeArguments);
    }
    @Test
    public void parseWrongNumberOfTypeArguments2() throws Exception {
        SyntaxError e = parseSyntaxError("WrongNumberOfTypeArguments2.front");
        assertTrue(e instanceof WrongNumberOfTypeArguments);
    }
}
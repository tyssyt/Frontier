package tys.frontier.parser;

import com.google.common.collect.Iterables;
import org.junit.Before;
import org.junit.Test;
import tys.frontier.code.FField;
import tys.frontier.code.FType;
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
    public void parseAbstractClassConstructorCall() throws Exception {
        SyntaxError e = parseSyntaxError("AbstractClassConstructorCall.front");
        assertTrue(e instanceof AbstractClassConstructorCall);
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
        assertTrue(c.a instanceof FType);
        assertTrue(c.b instanceof FType);
    }
    @Test
    public void parseContinueOutsideLoop() throws Exception {
        SyntaxError e = parseSyntaxError("ContinueOutsideLoop.front");
        assertTrue(e instanceof StatementOutsideLoop);
    }
    @Test
    public void parseCyclicClassHierachy() throws Exception {
        SyntaxError e = parseSyntaxError("CyclicClassHierachy.front");
        assertTrue(e instanceof CyclicClassHierachy);
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
    public void parseIncompatibleTypesForEach() throws Exception {
        SyntaxError e = parseSyntaxError("IncompatibleTypesForEach.front");
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
    /* TODO this test actually fails in the parser because our grammar does not allow fields in an interface
    @Test
    public void parseInterfaceInstanceField() throws Exception {
        SyntaxError e = parseSyntaxError("InterfaceInstanceField.front");
        assertTrue(e instanceof InterfaceInstanceField);
    }
    */
    @Test
    public void parseInterfaceSuperClass() throws Exception {
        SyntaxError e = parseSyntaxError("InterfaceSuperClass.front");
        assertTrue(e instanceof InterfaceSuperClass);
    }
    @Test
    public void parseMultiExtend() throws Exception {
        SyntaxError e = parseSyntaxError("ClassExtendedTwice.front");
        assertTrue(e instanceof MultiExtend);
    }
    @Test
    public void parseNoOverride() throws Exception {
        SyntaxError e = parseSyntaxError("NoOverride.front");
        assertTrue(e instanceof NoOverride);
    }
    @Test
    public void parseOverridesWithLessVisibility() throws Exception {
        SyntaxError e = parseSyntaxError("OverridesWithLessVisibility.front");
        assertTrue(e instanceof OverridesWithLessVisibility);
    }
    @Test
    public void parsePrivateInterface() throws Exception {
        SyntaxError e = parseSyntaxError("PrivateInterface.front");
        assertTrue(e instanceof PrivateInterface);
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
    public void parseTwoSuperClasses() throws Exception {
        SyntaxError e = parseSyntaxError("TwoSuperClasses.front");
        assertTrue(e instanceof TwoSuperClasses);
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

}
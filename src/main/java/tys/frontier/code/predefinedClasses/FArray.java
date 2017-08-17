package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FArrayIdentifier;
import tys.frontier.parser.syntaxTree.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxTree.syntaxErrors.SignatureCollision;

public class FArray extends FClass {

    private FClass baseClass;

    public FArray(FClass baseClass) {
        super(new FArrayIdentifier(baseClass.getIdentifier()), FVisibilityModifier.PUBLIC);
        this.baseClass = baseClass;
    }

    public static FArray createMultiArray(FClass baseClass, int arrayDepth) {
        FArray cur = new FArray(baseClass);
        for (int i=1; i<arrayDepth; i++) {
            cur = new FArray(cur);
        }
        return cur;
    }

    public FClass getBaseClass() {
        return baseClass;
    }

    @Override
    public void addField(FField field) throws IdentifierCollision {
        throw new RuntimeException("somthing went terribly wrong");
    }

    @Override
    public void addFunction(FFunction function) throws SignatureCollision {
        throw new RuntimeException("somthing went terribly wrong");
    }
}

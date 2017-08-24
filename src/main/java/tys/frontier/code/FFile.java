package tys.frontier.code;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.util.StringBuilderToString;

public class FFile implements StringBuilderToString {

    private String name;
    //imports go here
    private ImmutableMap<FClassIdentifier, FClass> classes;

    public FFile(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public FClass getClass(FClassIdentifier identifier) {
        return classes.get(identifier);
    }

    public ImmutableMap<FClassIdentifier, FClass> getClasses() {
        return classes;
    }

    public void setClasses(ImmutableMap<FClassIdentifier, FClass> classes) {
        assert this.classes == null;
        this.classes = classes;
    }

    public StringBuilder summary(StringBuilder sb) {
        for (FClass clazz : classes.values()) {
            clazz.summary(sb).append("\n\n");
        }
        return sb;
    }
    public String summary() {
        return summary(new StringBuilder()).toString();
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        for (FClass clazz : classes.values()) {
            clazz.toString(sb).append("\n\n");
        }
        return sb;
    }
    @Override
    public String toString() {
        return tS();
    }
}

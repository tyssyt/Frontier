package tys.frontier.code;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.visitor.FileVisitor;
import tys.frontier.util.StringBuilderToString;

import java.util.ArrayList;
import java.util.List;

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

    public <F,C,Fi,Fu,S,E> F accept(FileVisitor<F,C,Fi,Fu,S,E> visitor) {
        visitor.enterFile(this);
        List<C> classes = new ArrayList<>(this.classes.size());
        for (FClass c : this.classes.values())
            classes.add(c.accept(visitor));
        return visitor.exitFile(this, classes);
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

package tys.frontier.code;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.visitor.FileVisitor;
import tys.frontier.util.StringBuilderToString;

import java.util.ArrayList;
import java.util.List;

public class FFile implements StringBuilderToString {

    private String name;
    //imports go here
    private ImmutableMap<FTypeIdentifier, FType> types;

    public FFile(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public FType getClass(FTypeIdentifier identifier) {
        return types.get(identifier);
    }

    public ImmutableMap<FTypeIdentifier, FType> getTypes() {
        return types;
    }

    public void setTypes(ImmutableMap<FTypeIdentifier, FType> types) {
        assert this.types == null;
        this.types = types;
    }

    public <F,C,Fi,Fu,S,E> F accept(FileVisitor<F,C,Fi,Fu,S,E> visitor) {
        visitor.enterFile(this);
        List<C> classes = new ArrayList<>(this.types.size());
        for (FType t : this.types.values()) {
            classes.add(t.accept(visitor));
        }
        return visitor.exitFile(this, classes);
    }

    public StringBuilder summary(StringBuilder sb) {
        for (FType clazz : types.values()) {
            clazz.summary(sb).append("\n\n");
        }
        return sb;
    }
    public String summary() {
        return summary(new StringBuilder()).toString();
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        for (FType clazz : types.values()) {
            clazz.toString(sb).append("\n\n");
        }
        return sb;
    }
    @Override
    public String toString() {
        return tS();
    }
}

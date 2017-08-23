package tys.frontier.code;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.identifier.FClassIdentifier;

public class FFile {

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
}

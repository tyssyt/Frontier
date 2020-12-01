package tys.frontier.code.function;

import java.util.Optional;

public class NativeDecl {

    private String value;

    public NativeDecl(String value) {
        this.value = value;
    }

    public Optional<String> getValue() {
        return Optional.ofNullable(value);
    }
}

package tys.frontier.code;

import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface HasTypeParameters<T extends HasTypeParameters> extends IdentifierNameable {

    Map<FTypeIdentifier, FTypeVariable> getParameters();

    List<FTypeVariable> getParametersList();

    default T getInstantiation(List<FType> types) throws WrongNumberOfTypeArguments {
        if (getParametersList().size() != types.size()) {
            throw new WrongNumberOfTypeArguments(this, types);
        }
        if (types.size() == 0)
            return (T) this;
        Map<FTypeVariable, FType> typeMap = new HashMap<>();
        for (int i = 0; i < getParametersList().size(); i++) {
            if (getParametersList().get(i) != types.get(i))
                typeMap.put(getParametersList().get(i), types.get(i));
        }
        return getInstantiation(TypeInstantiation.create(typeMap));
    }

    T getInstantiation(TypeInstantiation typeInstantiation);
}

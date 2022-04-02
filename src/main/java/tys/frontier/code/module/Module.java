package tys.frontier.code.module;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.visitor.ModuleVisitor;
import tys.frontier.code.visitor.ModuleWalker;

import java.util.*;

import static tys.frontier.util.Utils.map;

public interface Module {

    String getName();
    List<Module> getImports();
    List<Include> getNativeIncludes();
    Map<FIdentifier, DefaultNamespace> getNamespaces();
    Map<FIdentifier, DefaultNamespace> getExportedNamespaces();

    default List<Module> findImportedModulesReflexiveTransitive() {
        List<Module> res = new ArrayList<>();
        Queue<Module> toDo = new ArrayDeque<>();
        toDo.add(this);
        while (!toDo.isEmpty()) {
            Module cur = toDo.remove();
            res.add(cur);
            toDo.addAll(cur.getImports());
        }
        return res;
    }

    default <M,N,C,Fi,Fu,S,E> M accept(ModuleWalker<M, N, C, Fi, Fu, S, E> walker) {
        return walker.enterModule(this);
    }

    default <M,N,C,Fi,Fu,S,E> M accept(ModuleVisitor<M, N, C, Fi, Fu, S, E> visitor) {
        visitor.enterModule(this);
        return visitor.exitModule(this, map(getNamespaces().values(), ns -> ns.accept(visitor)));
    }
}

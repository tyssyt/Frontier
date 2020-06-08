package tys.frontier.code.module;

import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.visitor.ModuleVisitor;
import tys.frontier.code.visitor.ModuleWalker;

import java.util.*;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

public interface Module {
    FFunction findMain() throws IllegalArgumentException, NoSuchElementException;

    Map<FIdentifier, DefaultNamespace> getExportedNamespaces();

    DefaultNamespace getNamespace(FIdentifier identifier);

    Stream<DefaultNamespace> getNamespaces();

    List<Include> getNativeIncludes();

    List<Module> getImports();

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
        List<N> ns = getNamespaces().map(namespace -> namespace.accept(visitor)).collect(toList());
        return visitor.exitModule(this, ns);
    }
}

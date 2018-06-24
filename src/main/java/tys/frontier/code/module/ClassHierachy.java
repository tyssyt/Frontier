package tys.frontier.code.module;

import com.google.common.graph.AbstractGraph;
import com.google.common.graph.ElementOrder;
import tys.frontier.code.FClass;
import tys.frontier.util.DisjunctUnionSetView;

import javax.annotation.ParametersAreNonnullByDefault;
import java.util.HashSet;
import java.util.Set;

public class ClassHierachy extends AbstractGraph<FClass> {

    private static final ElementOrder<FClass> nodeOrder = ElementOrder.unordered();

    private Set<FClass> classes;

    public ClassHierachy(Set<FClass> classes) {
        this.classes = classes;
    }

    public ClassHierachy() {
        this.classes = new HashSet<>();
    }

    public boolean add(FClass fClass) {
        return classes.add(fClass);
    }

    @Override
    public Set<FClass> nodes() {
        return classes;
    }

    @Override
    public boolean isDirected() {
        return true;
    }

    @Override
    public boolean allowsSelfLoops() {
        return false;
    }

    @Override
    public ElementOrder<FClass> nodeOrder() {
        return nodeOrder;
    }

    @Override
    @ParametersAreNonnullByDefault
    public Set<FClass> adjacentNodes(FClass node) {
        return DisjunctUnionSetView.of(node.getSuperClasses(), node.getSubClasses());
    }

    @Override
    @ParametersAreNonnullByDefault
    public Set<FClass> predecessors(FClass node) {
        return node.getSuperClasses();
    }

    @Override
    @ParametersAreNonnullByDefault
    public Set<FClass> successors(FClass node) {
        return node.getSubClasses();
    }
}

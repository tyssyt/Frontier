package tys.frontier.code.module;

import com.google.common.graph.AbstractGraph;
import com.google.common.graph.ElementOrder;
import tys.frontier.code.FType;
import tys.frontier.util.DisjunctUnionSetView;

import javax.annotation.ParametersAreNonnullByDefault;
import java.util.HashSet;
import java.util.Set;

public class ClassHierachy extends AbstractGraph<FType> {

    private static final ElementOrder<FType> nodeOrder = ElementOrder.unordered();

    private Set<FType> classes;

    public ClassHierachy(Set<FType> classes) {
        this.classes = classes;
    }

    public ClassHierachy() {
        this.classes = new HashSet<>();
    }

    public boolean add(FType fType) {
        return classes.add(fType);
    }

    @Override
    public Set<FType> nodes() {
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
    public ElementOrder<FType> nodeOrder() {
        return nodeOrder;
    }

    @Override
    @ParametersAreNonnullByDefault
    public Set<FType> adjacentNodes(FType node) {
        return DisjunctUnionSetView.of(node.getSuperTypes(), node.getSubTypes());
    }

    @Override
    @ParametersAreNonnullByDefault
    public Set<FType> predecessors(FType node) {
        return node.getSuperTypes();
    }

    @Override
    @ParametersAreNonnullByDefault
    public Set<FType> successors(FType node) {
        return node.getSubTypes();
    }
}

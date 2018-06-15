package tys.frontier.code.statement;

import com.google.common.collect.Iterables;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

public class FBlock implements FStatement, List<FStatement> {

    private List<FStatement> statements;

    public static FBlock from(List<FStatement> statements) {
        return new FBlock(statements);
    }

    public static FBlock empty() {
        return new FBlock(Collections.emptyList());
    }

    private FBlock(List<FStatement> statements) {
        assert statements.stream().limit(statements.size()-1).noneMatch( s -> s.redirectsControlFlow().isPresent());
        this.statements = statements;
    }

    public void setStatements(List<FStatement> statements) {
        this.statements = statements;
    }

    public List<FStatement> flattenRecursive() {
        List<FStatement> res = new ArrayList<>();
        for(FStatement statement : statements) {
            if (statement instanceof FBlock)
                res.addAll(((FBlock) statement).flattenRecursive());
            else
                res.add(statement);
        }
        return res;
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return Iterables.getLast(statements).redirectsControlFlow();
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterBlock(this);
        List<S> statements = new ArrayList<>(this.statements.size());
        for (FStatement s : this.statements)
            statements.add(s.accept(visitor));
        return visitor.exitBlock(this, statements);
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitBlock(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("{\n");
        for (FStatement statement : statements)
            statement.toString(sb).append('\n');
        return sb.append('}');
    }
    @Override
    public String toString() {
        return tS();
    }

    //start of delegated methods

    @Override
    public int size() {
        return statements.size();
    }

    @Override
    public boolean isEmpty() {
        return statements.isEmpty();
    }

    @Override
    public boolean contains(Object o) {
        return statements.contains(o);
    }

    @Override
    public Iterator<FStatement> iterator() {
        return statements.iterator();
    }

    @Override
    public Object[] toArray() {
        return statements.toArray();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        return statements.toArray(a);
    }

    @Override
    public boolean add(FStatement fStatement) {
        return statements.add(fStatement);
    }

    @Override
    public boolean remove(Object o) {
        return statements.remove(o);
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        return statements.containsAll(c);
    }

    @Override
    public boolean addAll(Collection<? extends FStatement> c) {
        return statements.addAll(c);
    }

    @Override
    public boolean addAll(int index, Collection<? extends FStatement> c) {
        return statements.addAll(index, c);
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        return statements.removeAll(c);
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        return statements.retainAll(c);
    }

    @Override
    public void replaceAll(UnaryOperator<FStatement> operator) {
        statements.replaceAll(operator);
    }

    @Override
    public void sort(Comparator<? super FStatement> c) {
        statements.sort(c);
    }

    @Override
    public void clear() {
        statements.clear();
    }

    @Override
    public FStatement get(int index) {
        return statements.get(index);
    }

    @Override
    public FStatement set(int index, FStatement element) {
        return statements.set(index, element);
    }

    @Override
    public void add(int index, FStatement element) {
        statements.add(index, element);
    }

    @Override
    public FStatement remove(int index) {
        return statements.remove(index);
    }

    @Override
    public int indexOf(Object o) {
        return statements.indexOf(o);
    }

    @Override
    public int lastIndexOf(Object o) {
        return statements.lastIndexOf(o);
    }

    @Override
    public ListIterator<FStatement> listIterator() {
        return statements.listIterator();
    }

    @Override
    public ListIterator<FStatement> listIterator(int index) {
        return statements.listIterator(index);
    }

    @Override
    public List<FStatement> subList(int fromIndex, int toIndex) {
        return statements.subList(fromIndex, toIndex);
    }

    @Override
    public Spliterator<FStatement> spliterator() {
        return statements.spliterator();
    }

    @Override
    public boolean removeIf(Predicate<? super FStatement> filter) {
        return statements.removeIf(filter);
    }

    @Override
    public Stream<FStatement> stream() {
        return statements.stream();
    }

    @Override
    public Stream<FStatement> parallelStream() {
        return statements.parallelStream();
    }

    @Override
    public void forEach(Consumer<? super FStatement> action) {
        statements.forEach(action);
    }
}

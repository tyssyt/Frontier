package tys.frontier.util;

import java.util.*;

public class MapStack<K,V> implements Iterable<Map<K,V>> {

    private Deque<Map<K, V>> stack = new ArrayDeque<>();

    //basic stack operations
    public void push(Map<K, V> map) {
        stack.push(map);
    }

    public void push() {
        stack.push(new HashMap<>());
    }

    public Map<K,V> pop() {
        return stack.pop();
    }

    public Map<K,V> peek() {
        return stack.peek();
    }

    public int stackHeight() {
        return stack.size();
    }

    public boolean isEmpty() {
        return stack.isEmpty();
    }

    public void clear() {
        stack.clear();
    }

    //operations for set elements
    public boolean contains(K t) {
        for (Map<K,V> map : this)
            if (map.containsKey(t))
                return true;
        return false;
    }

    public V put(K key, V value) {
        return stack.element().put(key, value);
    }

    public void putAll(Map<? extends  K, ? extends V> map) {
        stack.element().putAll(map);
    }

    public V get(K key) {
        for (Map<K,V> map : this) {
            V v = map.get(key);
            if (v != null)
                return v;
        }
        return null;
    }


    //iterators
    @Override
    public Iterator<Map<K, V>> iterator() {
        return stack.iterator();
    }


}
